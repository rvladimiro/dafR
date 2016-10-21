#' @export
#' @name MySQLQuery
#' @title Query data from a MySQL server
#' @author Ricardo Vladimiro
#' @description This function performs all the actions needed to run a query that returns tabular data from a MySQL server. It handles connecting, querying and disconnecting. The query can either be passed directly as a string or as a path to a file containing a SQL statement.
#' 
#' The yaml file must contain host, dbname, user and password. Port is optional, if not present in the yaml file, the default for MySQL will be used.
#' @param query The query to be ran. It can be a file with SQL or a character vector
#' @param id The server ID in the yaml config file
#' @param yamlFile The server configuration yaml file
#' @return A dataframe with the result of the query
MySQLQuery <- function(query, id, yamlFile = '../db.yml') {
    
    # Load and error checking --------------------------------------------------
    
    # Get the MySQL server configuration
    config <- GetDBConfig(id, yamlFile)
    
    # Assign default MySQL port if it doesn't exist
    if(is.null(config$port)) {
        Say('Port not found for server ID', id, '. Using default 3306')
        config$port = 3306
    }
    
    # Get the clean query
    query <- GetQueryStatement(query)
    
    # Run the query ------------------------------------------------------------
    
    # Create the connection to MySQL server
    connection <- RMySQL::dbConnect(
        drv = RMySQL::MySQL(),
        host = config$host,
        port = config$port,
        dbname = config$dbname,
        username = config$user,
        password = config$password
    )
    
    # Get data in chunks of 1000 rows
    dataframe <- data.table()
    results <- suppressWarnings(RMySQL::dbSendQuery(conn = connection, 
                                                    statement = query))
    while(!RMySQL::dbHasCompleted(res = results)) {
        dataframe <- data.table::rbindlist(list(
            dataframe,
            as.data.table(
                suppressWarnings(RMySQL::fetch(res = results, n = 1000))
            )
        ))
        Windmill("Fetched", nrow(dataframe), "rows")
    }
    
    # Clean up -----------------------------------------------------------------
    
    RMySQL::dbClearResult(results)
    RMySQL::dbDisconnect(connection)
    
    # Return data --------------------------------------------------------------
    
    return(dataframe)

}
