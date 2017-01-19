#' @export
#' @name RedshiftQuery
#' @title Query data from a Redshift dbID
#' @author Henrique Cabral and Ricardo Vladimiro
#' @description This functions runs queries pn Redshift via the UNLOAD command.
#' The query is unloaded to an s3 bucket and then read locally.
#' It handles connecting, querying, reading from s3 and disconnecting. The query can either be passed directly as a string or as a path to a file containing a SQL statement.
#' 
#' The yaml file must contain host, dbname, s3 bucket name, user and password.
#' 
#' This function, as opposed to PostgreSQLQuery, is intended to run large queries that might return a large amount of data or take some time to complete.
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
    s3FilePrefix <- paste0(GetProjectName(),
                           "-",
                           format(Sys.time(), "%Y%m%d-%H%M%S"))
    s3FilePrefix <- gsub(" ", "_", s3FilePrefix)

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
            "DELIMITER ';' ALLOWOVERWRITE;"
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

    # Read objects from S3
    nOfFiles <- length(s3FileList)
    
    Say("Copying files from S3:")
    
    # Read all objects from s3 by using path on file list
    s3Objects <- sapply(
        seq_len(nOfFiles),
        FUN = function(i) {
            Windmill("Copying file", i, "of", nOfFiles)
            aws.s3::get_object(s3FileList[i])
        }
    )
    
    # Exlude all objects with 0 bites, i.e, length 0
    s3Objects <- s3Objects[sapply(s3Objects, FUN = function(x) length(x) > 0)]
    
    
    # Read objects as data.frame
    Say("Reading files as data.table:")
    
    nOfObjs <- length(s3Objects)
    
    s3DataFrames <- lapply(
        seq_len(nOfObjs),
        FUN = function(n) {
            Windmill("Reading", n, "of", nOfObjs)
            iotools::read.delim.raw(
                rawConnection(s3Objects[[n]]),
                sep = ';', header = FALSE
            )
        }
    )
    
    # Close all opened connections
    closeAllConnections()
    
    # Create data.table
    Say("Creating data frame...")
    dt <- data.table::rbindlist(s3DataFrames)
    
    
    # Find and fix int64 columns
    int64Columns <- grep("integer64", sapply(dt, class))
    if(length(int64Columns) > 0) {
        for(column in int64Columns) {
            dt[[column]] <- as.integer(dt[[column]])
        }
    }

    
    # Finally delete files from s3
    Say("Removing files from s3")
    delRes <- sapply(
        seq_len(nOfFiles),
        FUN = function(i) {
            Windmill("Deleting file", i, "of", nOfFiles)
            aws.s3::delete_object(s3FileList[i])
        }
    )
    
    
    return(dt)

}
