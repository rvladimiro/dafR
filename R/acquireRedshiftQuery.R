#' @export
#' @name RedshiftQuery
#' @title Query data from a Redshift database
#' @author Henrique Cabral, Ricardo Vladimiro and João Monteiro
#' @description This functions runs queries pn Redshift via the UNLOAD command.
#' The query is unloaded to an s3 bucket and then read locally.
#' It handles connecting, querying, reading from s3 and disconnecting. The query can either be passed directly as a string or as a path to a file containing a SQL statement.
#' 
#' The yaml file must contain host, dbname, s3 bucket name, user and password.
#' 
#' This function, as opposed to PostgreSQLQuery, is intended to run large queries that might return a large amount of data or take some time to complete.
#' @param query Character vector with length 1. Can be either a SQL query or a path to a text file containing a SQL query
#' @param dbID The name of the yaml group containing the database credentials
#' @param s3ID The name of the yaml group containing the s3 bucket credentials
#' @param yamlConfig The path to the yaml file
#' @param acceleration Use s3 bucket acceleration
#' @param parallel Use parallel package for simultaneous connections to s3
#' @return The function will return a data.table object with the results of the query. In the case of a timeout it will return a string with the path of the created files on the s3 bucket
RedshiftQuery <- function(query, 
                          dbID, 
                          s3ID, 
                          yamlConfig = '~/db.yml',
                          acceleration = TRUE,
                          parallel = TRUE) {

    # Error checking and loading -----------------------------------------------

    # Get the dbID serverConfiguration
    dbConfig <- GetDBConfig(dbID, yamlConfig)

    # Assign default MySQL port if it doesn't exist
    if(is.null(dbConfig$port)) {
        Say('Port not found for dbID', dbID, '. Using default 5439')
        dbConfig$port = 5439
    }

    # Get the s3ID configuration
    # read s3 credentials
    s3Config <- GetS3Config(s3ID, yamlConfig)
    
    # set credentials on environment variables for 
    # compaitibility with aws.s3 package
    Sys.setenv(
        "AWS_ACCESS_KEY_ID" = s3Config$accessKey,
        "AWS_SECRET_ACCESS_KEY" = s3Config$secretKey,
        "AWS_DEFAULT_REGION" = ifelse(
            'region' %in% names(s3Config),
            s3Config$region,
            'us-west-2'
        )
    )
    
    # Prefixes for files to be created on the s3 buckets
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
        format(Sys.time(), "%Y%m%d-%H%M%S")
    )
    s3FilePrefix <- gsub(" ", "_", s3FilePrefix)
    s3FilePrefix <- gsub("[.]", "_", s3FilePrefix)

    # Get the clean query statement
    # - The 1st GetQueryStatement will return the intended query
    # - Next we escape quotes
    # - The 2nd GetQueryStatement will return the version with the unload code.
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
    
    # register start time to identify timeout errors
    queryStartTime <- Sys.time()
    
    # Hold on to error code in case this goes boom!
    errorCode <- try(
        results <- RPostgreSQL::dbSendQuery(conn = connection,
                                            statement = query)
    )
    
    # Calculate execution time
    queryExecTime <- abs(difftime(queryStartTime, Sys.time(), units = 'mins'))
    
    # If the query fails then return the s3 for manual download
    if (class(errorCode) == 'try-error') {
        if (queryExecTime > 10) {
            
            Shout(paste(
                'Query timed out.\n',
                'Use the returned string to fetch data from s3 once the query completes'
            ))
            
            return(paste0(
                s3Config$folder, s3FilePrefix
            ))
            
        } else {
            stop('Query Failed')
        }
        
    }
    
    # print out execution time
    Say(paste(
        'Query completed in', round(queryExecTime, 2), 'mins'
    ))
    
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
                "AND line_count > 0",
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
    
    # Get data frame from s3 bucket
    dt <- S3GetUnload(
        s3FileList,
        dbID = dbID,
        s3ID = s3ID,
        yamlConfig = yamlConfig,
        acceleration = acceleration,
        parallel = parallel
    )
    
    
    return(dt)

}
