#' @name CheckProjectDirectory
#' @author Ricardo Vladimiro
#' @title Checks if the project directory exists in the working directory and throws an error if it doesn't.
CheckProjectDirectory <- function() {
    if(!"./project" %in% list.dirs()) {
        stop(paste(
            "Project directory not found.",
            "Run CreateProjectStructure() to create the project's structure.",
            "Optionally, create the project directory manually."
        ))
    }
}