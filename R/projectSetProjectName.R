#' @export
#' @name SetProjectName
#' @title Sets the internal name used by dafR to reference the project
#' @author Ricardo Vladimiro
#' @description This function allows you to define the project name you want instead of leaving that to the automation of GetProjectName. If there's an existing project name, it will be substituted by this one, so keep in mind that some expected behaviours, for instance, restoring data from S3 may not be exactly what you'd expect.
#' 
#' If you wish to manually set the project name do so before using any function that fetchs it using GetProjectName.
#' 
#' The functions that use GetProjectName are:
#' - RedshiftQuery()
SetProjectName <- function(projectName) {
    .dafRVars$projectName <<- projectName
    SaveProjectEnvironment()
}