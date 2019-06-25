#' @export
#' @name GetQueryStatement
#' @author Ricardo Vladimiro
#' @title Get a clean query statement from a SQL file or string
#' @param query The query to be ran. It can be a file with SQL or a character vector
#' @return A character vector ready to be used on queries
GetQueryStatement <- function(query) {
    
    # Confirm it's a string
    if(!is.character(query)) {
        stop("Query must be a string with filename or SQL statement.")
    }
    
    # If the query is a SQL file, load it. Otherwise the character vector will
    # be interpreted as the SQL statement.
    if(file.exists(query)) {
        # Load the query
        query <- readLines(con = query)
        # Remove one line comments since they break the script
        query <- gsub(pattern = "--.*", replacement = "", x = query)
        #
        query <-
            # Put together all lines of the files
            paste(
                # strwrap will ensure that no special chars are present
                strwrap(x = query, width = .Machine$integer.max),
                collapse = ' ')
    }
    
    # Return the final query
    return(query)
}