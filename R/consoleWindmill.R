#' @export
#' @name Windmill
#' @title Update something in the terminal.
#' @description Say something in the same line while updating a windmill text animation. Serves to give visual feedback.
#' @param ... Variables to be sent to the console
Windmill <- function(...) {
    # Create the windmill
    windmill <- c("|", "/", "-", "\\")
    # Print the sentence
    sentence <- as.character(unlist(list(...)))
    cat("\r", paste(sentence), windmill[.dafRVars$lastWindmillChar])
    # Update environment variables
    .dafRVars$lastWindmillChar <<-
        ifelse(test = .dafRVars$lastWindmillChar == 4,
               yes = 1, 
               no = .dafRVars$lastWindmillChar + 1)
                                       
    .dafRVars$wasUpdate <<- T
    # Return status
    invisible(TRUE)
}