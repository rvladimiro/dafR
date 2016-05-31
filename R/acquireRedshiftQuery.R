#' @export
#' @name RedshiftQuery
#' @title Query data from a Redshift dbID
#' @author Henrique Cabral and Ricardo Vladimiro
#' @description Describe this thing
RedshiftQuery <- function(query, dbID, s3ID, yamlConfig = '../db.yml') {
    
    # Error checking and loading -----------------------------------------------
    
    # Get the dbID serverConfiguration
    dbConfig <- GetDBConfig(dbID, yamlConfig)
    
    # Assign default MySQL port if it doesn't exist
    if(is.null(dbConfig$port)) {
        Say('Port not found for dbID', dbID, '. Using default 5439')
        dbConfig$port = 5439
    }
    
    # Get the s3ID configuration
    s3Config <- GetS3Config(s3ID, yamlConfig)
    s3FilePrefix <- paste0(GetProjectName(), 
                           "-",
                           format(Sys.time(), "%Y%m%d-%H%M%S"))
    
    #' Get the clean query statement
    #' - The 1st GetQueryStatement will return the intended query
    #' - Next we escape quotes
    #' - The 2nd GetQueryStatement will return the version with the unload code.
    query <- GetQueryStatement(query)
    query <- gsub(pattern = "\'", replacement = "\'\'", x = query)
    query <- GetQueryStatement(
        # It's written like this just to be readable, k?!
        paste0(
            "UNLOAD('", query, "')",
            "TO '", s3Config$folder, s3FilePrefix, "_'",
            "CREDENTIALS '",
            "aws_access_key_id=", s3Config$accessKey, ";",
            "aws_secret_access_key=", s3Config$secretKey, "'",
            "DELIMITER ';' GZIP ALLOWOVERWRITE;"
        )
    )
    
    # Run the query ------------------------------------------------------------
    
    # Create connection to Redshift dbID
    connection <- RPostgreSQL::dbConnect(
        drv = RPostgreSQL::PostgreSQL(),
        host = dbConfig$host,
        port = dbConfig$port,
        dbname = dbConfig$dbname,
        user = dbConfig$user,
        password = dbConfig$password
    )
    
    # Get the error code (if any) from the query
    Say("Running query.")
    # Hold on to error code in case this goes boom!
    errorCode <- try(
        results <- RPostgreSQL::dbSendQuery(conn = connection,
                                            statement = query)
    )
    
    # Get the query ID
    Say("Getting query ID.")
    queryIDResults <- RPostgreSQL::dbSendQuery(
        conn = connection, 
        statement = "select pg_last_query_id();"
    )
    queryID <- RPostgreSQL::fetch(
        res = queryIDResults
    )
    
    # Get file list
    Say("Getting S3 file list.")
    s3FileList <- RPostgreSQL::fetch(
        res = RPostgreSQL::dbSendQuery(
            conn = connection, 
            statement = paste(
                "SELECT path FROM stl_unload_log",
                "WHERE query =", queryID,
                "ORDER BY path;"
            )
        )
    )
    
    # Clean up database connections
    RPostgreSQL::dbClearResult(results)
    RPostgreSQL::dbClearResult(queryIDResults)
    RPostgreSQL::dbDisconnect(connection)
    
    # Get files from S3 --------------------------------------------------------
    
    # Remove extra spaces
    s3FileList <- gsub(pattern = " ", replacement = "", x = s3FileList$path)
    
    # Copy and delete files from S3
    Say("Copying files from S3:")
    nOfFiles <- length(s3FileList) 
    for(n in 1:nOfFiles) {
        Windmill("Copying file", n, "of", nOfFiles)
        rmtFile <- s3FileList[n] # Remote file
        lclFile <- paste0("data/s3", s3FilePrefix, "-", n, ".gz")
        # Get the file from S3 and save it on data folder
        # After fetch files are deleted
        system(
            paste(
                "s3cmd get", rmtFile, lclFile,
                paste0("--access_key=", s3Config$accessKey),
                paste0("--secret_key=", s3Config$secretKey),
                "--delete-after-fetch"
            ),
            ignore.stdout = T,
            ignore.stderr = F
        )
    }
    
    # Unzip files, create data.table and return it -----------------------------
    
    # Unzip files
    Say("Unzipping files:")
    fileList <- list.files(path = "data", pattern = s3FilePrefix)
    nOfFiles <- length(fileList)
    for(n in 1:nOfFiles) {
        Windmill("Unzipping", n, "of", nOfFiles)
        # gunzip removes files automatically
        R.utils::gunzip(filename = paste0("data/", fileList[n]), remove = T)
    }
    
    # Create data.table
    Say("Reading local files to data.table:")
    fileList <- list.files(path = "data", pattern = s3FilePrefix)
    # Remove 0 length files
    fileSizes <- file.size(paste0("data/", fileList))
    fileList <- fileList[fileSizes > 0]
    nOfFiles <- length(fileList)
    dt <- data.table::data.table()
    for(n in 1:nOfFiles) {
        Windmill("Reading", n, "of", nOfFiles)
        # Load the partial data table
        dt <- data.table::rbindlist(list(
            dt, 
            data.table::fread(paste0("data/", fileList[n]), header = F)
        ))
    }
    
    # Find and fix int64 columns
    int64Columns <- grep("integer64", sapply(dt, class))
    if(length(int64Columns) > 0) {
        for(column in int64Columns) {
            dt[[column]] <- as.integer(dt[[column]])
        }
    }
    
    # Delete the source file
    file.remove(paste0("data/", fileList))
    
    return(dt)
    
}