#' @export
#' @name Say
#' @author Ricardo Vladimiro
#' @title Say something in the console
#' @description Say has the same behavior as cat but adds a carriage return to the end. It will output a sentence built from the arguments passed. A carriage return is added for ease of use.
#' @param ... Variables to be sent to the console
Say <- function(...) {
    # Add a new line if the last console action was an update
    if(.dafRVars$wasUpdate) cat("\n")
    # Print the sentence with an added new line
    cat(paste(as.character(unlist(list(...)))), "\n")
    # This was not an update but a new line so save the state
    .dafRVars$wasUpdate <<- F
}