#' @export
#' @name Shout
#' @title Shout something in the console
#' @description Shout is a cat to stderr, meaning it will print like an error but without halting execution. It will output a sentence built from the arguments passed. A carriage return is added for ease of use.
#' @param ... Variables to be sent to the console
Shout <- function(...) {
    # Add a new line if the last console action was an update
    if(.dafRVars$wasUpdate) cat("\n")
    # Print the sentence with an added new line
    cat(paste(as.character(unlist(list(...)))), "\n", file = stderr())
    # This was not an update but a new line so save the state
    .dafRVars$wasUpdate <<- F
}