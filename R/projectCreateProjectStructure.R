#' @export
#' @author Ricardo Vladimiro, Daniela Cunha
#' @name CreateProjectStructure
#' @title Create base directories and files.
#' @description Creates directory code and data and git ignore file. The git ignore file is from https://github.com/github/gitignore/blob/master/R.gitignore plus any additions from us.
CreateProjectStructure <- function() {
    
    # Create code directory if missing
    if(!'./code' %in% list.dirs()) {
        dir.create('code')
        Say('Code directory created.')
    }
    
    # Create data directory if missing
    if(!'./data' %in% list.dirs()) {
        dir.create('data')
        Say('Data directory created.')
    }
    
    # Create project directory if missing
    if(!'./project' %in% list.dirs()) {
        dir.create('project')
        Say('Project directory created.')
    }
    
    # Create queries directory if missing
    if(!'./queries' %in% list.dirs()) {
        dir.create('queries')
        Say('Queries directory created.')
    }
    
    # Add .gitignore
    file.copy(
        from = paste0(path.package('dafR'), '/gitignore'),
        to = '.gitignore'
    )
    Say('.gitignore file added.')
}