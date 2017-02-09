#' @export
#' @name InsertDataFrameToRedshift
#' @title Creates a Redshift table with the same contents as the given R data frame
#' @author João Monteiro and Henrique Cabral
#' @description Given an R data frame, or equivalently, any data.frame like object, the function will create a redshift table tthat mimics the contents of the R object.
#' This copy is done by first copying then object onto and s3 bucket as a .csv, and then uploading to redshift from there.
#' 
#' The yaml file must contain host, dbname, s3 bucket name, user and password.
#' @param df a data.frame or data.frame like object
#' @param tableName The name of the redshift table to create or append to
#' @param columnTypes A character vector with the same length as the number of columns on df. Each element must be a Redshift compatible type. If not provided, te column types will be guessed automatically
#' @param schema Character vector with length 1 with the name of the DB schema were the table will be created
#' @param clean Boolean indicating whether or not to remove existing records from the table before inserting
#' @param dbID The name of the yaml group containing the database credentials
#' @param s3ID The name of the yaml group containing the s3 bucket credentials
#' @param yamlConfig The path to the yaml file
#' @return The function will return a boolean value indicating if the process completed successfully
InsertDataFrameToRedshift <- function(df, 
                                      tableName,
                                      columnTypes,
                                      schema = 'development',
                                      clean = TRUE,
                                      dbID, 
                                      s3ID, 
                                      yamlConfig = '../db.yml') {
    
    
    ## Write data frame as csv on the disk -------------------------------------
    # construct proper name
    csvName <- paste0(
        tableName, '_to_uploadS3_', format(Sys.time(), "%Y%m%d-%H%M%S"), '.csv'
    )
    
    # If 'data' folder is present write there. 
    # Otherwise write on working directory
    dataDirPresent <- 'data' %in% list.dirs(
        path = getwd(), full.names = FALSE, recursive = FALSE
    )
    
    if (dataDirPresent) 
        csvName <- paste0('data/', csvName)
    
    # write csv
    Say('Writing .csv')
    writeRes <- readr::write_csv(df, csvName)
    
    
    
    ## S3 upload ---------------------------------------------------------------
    # Initialize s3 credentials
    s3Config <- GetS3Config(s3ID, yamlConfig)
    
    Sys.setenv(
        "AWS_ACCESS_KEY_ID" = s3Config$accessKey,
        "AWS_SECRET_ACCESS_KEY" = s3Config$secretKey,
        "AWS_DEFAULT_REGION" = ifelse(
            'region' %in% names(s3Config),
            s3Config$region,
            'us-west-2'
        )
    )
    
    # Set a prefix for the path to write to
    # Use project name to identify path in s3
    # If project structure is non existent use a default name
    projName <- try(GetProjectName(), silent = TRUE)
    if (class(projName) == 'try-error')
        projName <- 'ad_hoc_queries'
    
    # remove all . and substitute with _
    queryUser <- gsub('[.]', '_', Sys.getenv('LOGNAME'))
    s3FilePrefix <- paste0(
        ifelse(
            queryUser == '',
            'unknown',
            queryUser
        ),
        "/",
        projName,
        "/",
        "csv_to_upload/"
    )
    s3FilePrefix <- gsub(" ", "_", s3FilePrefix)
    s3FilePrefix <- gsub("[.]", "_", s3FilePrefix)
    
    # put csv onto choosen path
    Say('Uploading to s3')
    s3PutCsv <- try(aws.s3::put_object(
        csvName,
        object = paste0(s3Config$folder, s3FilePrefix, csvName)
    ))
    
    if (class(s3PutCsv) == "try-error") {
        
        Shout('Error uploading file to S3')
        
        # remove local and remote files
        file.remove(csvName)    
        aws.s3::delete_object(
            object = paste0(s3Config$folder, s3FilePrefix, csvName)
        )
        return(FALSE)
        
    }
    
    # Remove local file
    file.remove(csvName)
    
    
    ## Check data types --------------------------------------------------------
    # If data types are not provided then try to guess them
    if (!hasArg(columnTypes)){
        
        # function to guess sql types based on dataframe column type
        guessTypes <- function(x) {
            if (is.integer(x)){
                return('int')
            } else if (is.numeric(x)){
                return('float')
            } else if (is.POSIXct(x) & mean(nchar(as.character(x)), na.rm = T) > 10) {
                return('datetime')
            } else if (is.POSIXct(x) | is.Date(x)) {
                return('date')
            } else {
                return('varchar(255)')
            }
        }
        
        # create vector of length 1 with column names and type
        columnTypes <- paste(
            paste(colnames(df), toupper(sapply(df, guessTypes))),
            collapse = ', '
        )
        
        Say('Guessed column types as:\n    ', columnTypes)
        
    } else {
        
        # make sure provided types correspond to the number of columns
        if (ncol(df) != length(columnTypes)) {
            Shout('Provided column types do not correspond to the number of columns in dataset')
            aws.s3::delete_object(
                object = paste0(s3Config$folder, s3FilePrefix, csvName)
            )
            return(FALSE)
        }
        
        # create vector of length 1 with column names and type
        columnTypes <- paste(
            paste(colnames(df), toupper(columnTypes)),
            collapse = ', '
        )
        
    }
    
    
    ## Create redshift table ---------------------------------------------------
    # Check first if table exists 
    existsQuery <- paste(
        "SELECT table_name",
        "FROM information_schema.tables",
        "WHERE table_schema = ", paste0("'", schema, "'"),
        "AND table_name = ", paste0("'", tableName, "'")
    )
    
    Say('Checking if table is present on given schema')
    tableExists <- try(PostgreSQLQuery(
        query = existsQuery,
        id = dbID, yamlFile = yamlConfig,
        printProgress = FALSE
    ))
    
    # If exactly one row is extracted then the table is present
    if (class(tableExists)[1] == 'try-error') {
        Shout('Error checking if table is present on given schema')
        aws.s3::delete_object(
            object = paste0(s3Config$folder, s3FilePrefix, csvName)
        )
        return(FALSE)
    } else {
        tableExists <- nrow(tableExists) == 1
    }
    
    # If table does not exist create a new one
    if (!tableExists) {
        
        # create statement
        queryCreate <- paste(
            "CREATE TABLE", paste(schema, tableName, sep = "."),
            "(", columnTypes, ")"
        )
        
        Say('Creating new table')
        createTable <- try(PostgreSQLQuery(
            query = queryCreate,
            id = dbID, yamlFile = yamlConfig,
            printProgress = FALSE
        ))
        
        if (class(createTable)[1] == "try-error") {
            Shout('Error creating new table')
            aws.s3::delete_object(
                object = paste0(s3Config$folder, s3FilePrefix, csvName)
            )
            return(FALSE)
        } 
        
        Say('Table created successfully')
        
        
    } else if (clean) {
        
        # Drop table if exists and clean ir requested
        Say('Dropping old table instance')
        dropTable <- try(PostgreSQLQuery(
            query = paste(
                "DROP TABLE", paste(schema, tableName, sep = ".")
            ),
            id = dbID, yamlFile = yamlConfig,
            printProgress = FALSE
        ))
        
        # Check for errors
        if (class(dropTable)[1] == "try-error") {
            Shout('Error dropping old table')
            aws.s3::delete_object(
                object = paste0(s3Config$folder, s3FilePrefix, csvName)
            )
            return(FALSE)
        }
        
        
        # create statement
        queryCreate <- paste(
            "CREATE TABLE", paste(schema, tableName, sep = "."),
            "(", columnTypes, ")"
        )
        
        Say('Creating new table')
        createTable <- try(PostgreSQLQuery(
            query = queryCreate,
            id = dbID, yamlFile = yamlConfig,
            printProgress = FALSE
        ))
        
        if (class(createTable)[1] == "try-error") {
            Shout('Error creating new table')
            aws.s3::delete_object(
                object = paste0(s3Config$folder, s3FilePrefix, csvName)
            )
            return(FALSE)
        } 
        
        Say('Table created successfully')
        
    }
    
    
    ## Copying to Redsfhit via s3 ----------------------------------------------
    # Query for copy command
    queryCopy <- paste0(
        " COPY ", paste(schema, tableName, sep = "."),
        " FROM ", paste0("'", s3Config$folder, s3FilePrefix, csvName, "'"),
        " CREDENTIALS ",
        "'aws_access_key_id=", s3Config$accessKey, ";",
        "aws_secret_access_key=", s3Config$secretKey, "'",
        " CSV ignoreheader 1 NULL 'NaN'"
    )
    
    Say('Uploading from s3 to Redshift')
    copyCsv <- try(PostgreSQLQuery(
        query = queryCopy,
        id = dbID, yamlFile = yamlConfig,
        printProgress = FALSE
    ))
    
    
    if (class(copyCsv)[1] == "try-error") {
        Shout('Error copying from s3 to Redshift')
        aws.s3::delete_object(
            object = paste0(s3Config$folder, s3FilePrefix, csvName)
        )
        return(FALSE)
    } 
    
    Say('Data uploaded successfully')
    
    Say('Removing .csv from s3')
    aws.s3::delete_object(
        object = paste0(s3Config$folder, s3FilePrefix, csvName)
    )

    return(TRUE)
    
}
