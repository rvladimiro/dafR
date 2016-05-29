#' @export
#' @name Clear
#' @title Clear the console because I always forget the proper escape code
#' @description I'm only writing a description to avoid a warning. I'm a nice package creator. Just type Clear() and the console will be cleared!
#' @author Ricardo Vladimiro
Clear <- function() {
    cat("\014") 
}