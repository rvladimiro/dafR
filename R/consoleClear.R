#' @export
#' @name Clear
#' @title Clear the console because I always forget the proper escape code
#' @author Ricardo Vladimiro
Clear <- function() {
    cat("\014") 
}