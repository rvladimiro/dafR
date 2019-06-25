#' @name CheckDataDirectory
#' @author Ricardo Vladimiro
#' @title Checks if the data directory exists in the working directory and throws an error if it doesn't.
CheckDataDirectory <- function() {
    if(!"./data" %in% list.dirs()) {
        stop(paste(
            "Data directory not found.",
            "Run CreateProjectStructure() to create the project's structure.",
            "Optionally, create the data directory manually."
        ))
    }
}