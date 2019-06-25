#' @export
#' @name ConvertToCamelCase
#' @title Convert a string from under_score syntax to camelCase
#' @description helper function to convert underscore names to camelCase, 
#'     to ease in following variable naming conventions.
#'     Copied from https://stackoverflow.com/questions/25504222/elegant-r-function-mixed-case-separated-by-periods-to-underscore-separated-lowe
#' @param s string vector to be converted
#' @return string vector with converted strings
ConvertToCamelCase <- function(s) {
    
    # apply regex
    gsub("_(.)", "\\U\\1", s, perl = TRUE)
    
}