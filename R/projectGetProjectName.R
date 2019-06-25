#' @export
#' @name GetProjectName
#' @title Get the internal name used by dafR to reference the project
#' @author Ricardo Vladimiro
#' @description GetProjectName serves two purposes: to get the reference that the package uses internaly and also to create that reference if it wasn't used yet. This reference is created and saved in the first time GetProjectName is called in the open project.
#' 
#' When the function is called, it will check if the reference exists. If it does, it simply returns it. If it doesn't it will look for the Rproj file of RStudio. If the file exists in the working directory GetProjectName will use the filename without the extension and save it.
#' 
#' If both searches fail, GetProjectName will use the working directory name as the project name.
GetProjectName <- function() {
    
    # If project name exists in .dafRVars return it
    if(!is.null(.dafRVars$projectName)) {
        return(.dafRVars$projectName)
    }
    
    #' If it doesn't get it from the Rproj file and save it in .dafRVars and 
    #' return it. If there's more than one Rproj file (why would that happen I
    #' don't really know) pick the first one.
    RprojFileList <- list.files(path = ".", pattern = ".Rproj$")
    if(length(RprojFileList) >= 1) {
        projectName <- gsub(pattern = ".Rproj$", 
                            replacement = "",
                            x = RprojFileList[1])
        SetProjectName(projectName)
        return(projectName)
    }
    
    #' If it doesn't get it from the working directory name, save it in
    #' .mrtEvn and return it
    workingDirectory <- tail(unlist(strsplit(getwd(), split = "/")), n = 1)
    SetProjectName(workingDirectory)
    return(workingDirectory)
    
}