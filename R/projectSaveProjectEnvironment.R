#' @name SaveProjectEnvironment
#' @title Saves the project environment in the project directory
#' @author Ricardo Vladimiro
SaveProjectEnvironment <- function() {
    CheckProjectDirectory()
    saveRDS(object = .dafRVars, file = "./project/dafRVars.rds")
}