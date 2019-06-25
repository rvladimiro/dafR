#' @export
#' @name ReplaceQueryParams
#' @author João Monteiro
#' @title Replaces \%s parameters in a query statement by the given arguments
#' @param query The query to be treated It can be a file with SQL or a character vector
#' @param ... a number of strings to be replaced in the query
#' 
#' @return A character vector ready to be used on queries
#' 
#' @examples 
#' ReplaceQueryParams(
#'     query = "SELECT * FROM %s_session",
#'     "game_name"
#' )
#' 
#' ReplaceQueryParams(
#'     query = "SELECT *, '%1$s' AS game 
#'     FROM %1$s_session
#'     WHERE date = '%2$s'",
#'     "game_name",
#'     Sys.Date()
#' )
ReplaceQueryParams <- function(query, ...) {
    
    # get the query statement
    query <- GetQueryStatement(query)
    
    # replace placeholders with given params
    query <- sprintf(query, ...)
    
    return(query)
    
}