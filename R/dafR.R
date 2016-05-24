# On package load

.onLoad <- function(libname, pkgname) {
    
    # Check if local dafR variables exist
    # - If they do, load them
    # - If they don't, create them
    if("dafRVars.rds" %in% list.files("project")) {
        vars <- readRDS("project/dafRVars.rds")
    } else {
        vars <- list(wasUpdate = F,
                     lastWindmillChar = 1)
    }
    
    # Assign it to a hidden global environment variable
    assign(x = ".dafRVars", value = vars, envir = globalenv())
    
}