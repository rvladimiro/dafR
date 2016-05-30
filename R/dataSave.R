#' @export
#' @name Save
#' @title Load and save R objects from and to the data directory
#' @rdname LoadAndSaveData
Save <- function(object) {
    
    # Get the name of the object
    objectName <- deparse(substitute(object))
    
    # Error Checking -----------------------------------------------------------
    # Stop if the object doesn't exist in the global environment
    # Stop if data directory doesn't exist in the work directory
    
    if(!objectName %in% ls(envir = .GlobalEnv)) {
        stop("Object not found in GlobalEnv!")
    }
    
    CheckDataDirectory()
    
    # Save the object ----------------------------------------------------------
    saveRDS(object = object, file = paste0("./data/", objectName, ".rds"))
}