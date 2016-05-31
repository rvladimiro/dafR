#' @export
#' @name PostgreSQLQuery
#' @title Query data from a PostgreSQL server
#' @author Ricardo Vladimiro
#' @description This function performs all the actions needed to run a query that returns tabular data from a PostgreSQL server. It handles connecting, querying and disconnecting. The query can either be passed directly as a string or as a path to a file containing a SQL statement.
#' 
#' The yaml file must contain host, dbname, user and password. Port is optional, if not present in the yaml file, the default for PostgreSQL will be used.
#' 
#' This function was created to run quick queries on Redshift. The difference between this function and RedshiftQuery is that RedshiftQuery was built to unload results to an S3 bucket and should the prefered option for large queries.
#' @param query The query to be ran. It can be a file with SQL or a character vector
#' @param id The server ID in the yaml config file
#' @param yamlFile The server configuration yaml file
#' @return A dataframe with the result of the query
PostgreSQLQuery <- function(query, id, yamlFile = '../db.yml') {
    
    # Load and error checking --------------------------------------------------
    
    # Get the PostgreSQL server configuration
    config <- GetDBConfig(id, yamlFile)
    
    # Assign default PostgreSQL port if it doesn't exist
    if(is.null(config$port)) {
        Say('Port not found for server ID', id, '. Using default 5432')
        config$port = 5432
    }
    
    # Get the clean query
    query <- GetQueryStatement(query)
    
    # Run the query ------------------------------------------------------------
    
    # Create the connection to the PostgreSQL server
    connection <- RPostgreSQL::dbConnect(
        drv = RPostgreSQL::PostgreSQL(),
        host = config$host,
        port = config$port,
        dbname = config$dbname,
        user = config$user,
        password = config$password
    )
    
    # Get data in chunks of 1000 rows
    dataframe <- data.frame()
    results <- suppressWarnings(RPostgreSQL::dbSendQuery(conn = connection, 
                                                         statement = query))
    
    while(!RPostgreSQL::dbHasCompleted(res = results)) {
        dataframe <- rbind(
            dataframe,
            suppressWarnings(RPostgreSQL::fetch(res = results, n = 1000))
        )
        Windmill("Fetched", nrow(dataframe), "rows")
    }
    
    # Clean up -----------------------------------------------------------------
    
    RPostgreSQL::dbClearResult(results)
    RPostgreSQL::dbDisconnect(connection)
    
    # Return data --------------------------------------------------------------
    
    return(dataframe)
    
    # Run the query and return the dataframe -----------------------------------

}
