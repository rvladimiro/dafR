#' @export
#' @name GetDBConfig
#' @author Ricardo Vladimiro
#' @title Get database server configuration from a given a yaml configuration file
#' @param id The id in the yaml config file
#' @param yamlFile The configuration yaml file
#' @return A list of configuration parameters
GetDBConfig <- function(id, yamlFile) {
    
    # Load the configuration
    config <- GetYAMLConfig(id, yamlFile)
    
    # Check if all id information needed is present
    nOfErrors = 0
    
    if(is.null(config$host)) {
        Shout('Hostname not present for id', id)
        nOfErrors = nOfErrors + 1
    }
    
    if(is.null(config$dbname)) {
        Shout('Database name not present for id', id)
        nOfErrors = nOfErrors + 1
    }
    
    if(is.null(config$user)) {
        Shout(paste('User not present for id', id))
        nOfErrors = nOfErrors + 1
    }
    
    if(is.null(config$password)) {
        Shout(paste('Password not present for id', id))
        nOfErrors = nOfErrors + 1
    }
    
    # If any error was found stop execution
    if(nOfErrors > 0) {
        stop(paste(nOfErrors, 'error(s) found. Review yaml config file.'))
    }
    
    # If not return the config
    return(config)
    
}