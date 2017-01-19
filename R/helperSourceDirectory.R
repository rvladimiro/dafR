#' @export
#' @name SourceDirectory
#' @title Source all R files of a directory
#' @author R Core Team and Ricardo Vladimiro
#' @description Taken from the source help file and slightly changed to fit dafR
SourceDirectory <- function(directoryPath, ...) {
    Say('Sourcing scripts at', directoryPath)
    for(filename in list.files(path = directoryPath, pattern = '[.][Rr]$')) {
        Say('    ', filename)
        source(file.path(directoryPath, filename), ...)
    }
}