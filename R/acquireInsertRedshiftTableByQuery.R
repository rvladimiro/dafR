#' @export
#' @name InsertRedshiftTableByQuery
#' @title Creates a table with the results of a given SQL query
#' @author João Monteiro
#' @description Given a SQL query, this functions inserts it's results onto a redshift table.
#' We can choose to clean the existing table records before inserting or to simply append.
#' If the table does not exist, one will be automatically created
#' 
#' The yaml file must contain host, dbname, user and password.
#' @param query Character vector with length 1. Can be either a SQL query or a path to a text file containing a SQL query
#' @param tableName The name of the redshift table to create or append to. Will be transformed to lower case.
#' @param schema Character vector with length 1 with the name of the DB schema were the table will be created
#' @param clean Boolean indicating whether or not to remove existing records from the table before inserting
#' @param id The name of the yaml group containing the database credentials
#' @param yamlFile The path to the yaml file
#' @return The function will return a boolean value indicating if the process completed successfully
InsertRedshiftTableByQuery <- function(query, 
                                       tableName,
                                       schema = 'development',
                                       clean = TRUE,
                                       id,                                        
                                       yamlFile = '../db.yml') {
    
    # Make sure table name is in lower cases to comply with Redsfhit naming
    tableName <- tolower(tableName)
    
    
    ## Check if the table already exists
    # query to check tables present on given schema
    existsQuery <- paste(
        "SELECT table_name",
        "FROM information_schema.tables",
        "WHERE table_schema = ", paste0("'", schema, "'"),
        "AND table_name = ", paste0("'", tableName, "'")
    )
    
    Say('Checking if table is present on given schema')
    tableExists <- try(PostgreSQLQuery(
        query = existsQuery,
        id = id, yamlFile = yamlFile,
        printProgress = FALSE
    ))
    
    # If exactly one row is extracted then the table is present
    if (class(tableExists)[1] == 'try-error') {
        Shout('Error checking if table is present on given schema')
        return(FALSE)
    } else {
        tableExists <- nrow(tableExists) == 1
    }
    
    
    # Get query for table to be inserted
    queryTable <- GetQueryStatement(query)
    
    
    # If table does not exist create a new one
    if (!tableExists) {
        
        # Expand statement with create table command
        queryCreate <- paste(
            "CREATE TABLE", paste(schema, tableName, sep = "."), "AS",
            "(", queryTable, ")"
        )
        
        # Create it
        Say('Creating new table')
        createTable <- try(PostgreSQLQuery(
            query = queryCreate,
            id = id, yamlFile = yamlFile,
            printProgress = FALSE
        ))
        
        if (class(createTable)[1] == "try-error") {
            Shout('Error creating new table')
            return(FALSE)
        } else {
            Say('Table created successfully')
            return(TRUE)
        }
        
    }
    
    
    # If clean is requestes first drop old instance
    # Otherwise append to existing table
    if (clean) {
        
        # Drop table
        Say('Dropping old table instance')
        dropTable <- try(PostgreSQLQuery(
            query = paste(
                "DROP TABLE", paste(schema, tableName, sep = ".")
            ),
            id = id, yamlFile = yamlFile,
            printProgress = FALSE
        ))
        
        # Check for errors
        if (class(dropTable)[1] == "try-error") {
            Shout('Error dropping old table')
        }
        
        
        # Expand statement with create table command
        queryCreate <- paste(
            "CREATE TABLE", paste(schema, tableName, sep = "."), "AS",
            "(", queryTable, ")"
        )
        
        # Create it
        Say('Creating new table')
        createTable <- try(PostgreSQLQuery(
            query = queryCreate,
            id = id, yamlFile = yamlFile,
            printProgress = FALSE
        ))
        
        if (class(createTable)[1] == "try-error") {
            Shout('Error creating new table')
            return(FALSE)
        } else {
            Say('Table created successfully')
            return(TRUE)
        }
        
    } else {
        
        # Expand statement with insert command
        queryInsert <- paste(
            "INSERT INTO", paste(schema, tableName, sep = "."), 
            "(", queryTable, ")"
        )
        
        # Insert
        Say('Inserting into existing table')
        insertTable <- try(PostgreSQLQuery(
            query = queryInsert,
            id = id, yamlFile = yamlFile,
            printProgress = FALSE
        ))
        
        if (class(insertTable)[1] == "try-error") {
            Shout('Error inserting into old table')
            return(FALSE)
        } else {
            Say('New data inserted successfully')
            return(TRUE)
        }
        
    }
    
    return(FALSE)
    
}
