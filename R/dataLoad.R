#' @export
#' @name Load
#' @title Load and save R objects from and to the data directory
#' @rdname LoadAndSaveData
Load <- function(object) {
    
    # Get the object name and create the filename
    objectName <- deparse(substitute(object))
    fileName <- paste0(objectName, ".rds")
    
    # Error checking -----------------------------------------------------------
    # Stop if the data directory doesn't exist in the work directory
    # Stop if the file doesn't exist
    
    CheckDataDirectory()
    
    if(!fileName %in% list.files("./data/")) {
        stop(paste("File", fileName, "doesn't exist"))
    }
    
    # Load the object into the Global Environment ------------------------------
    assign(
        x = objectName, 
        value = readRDS(file = paste0("./data/", fileName)), 
        envir = globalenv()
    )
}