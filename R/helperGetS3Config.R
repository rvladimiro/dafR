#' @export
#' @name GetS3Config
#' @author Ricardo Vladimiro
#' @title Get S3 server configuration from a given a yaml configuration file
#' @param id The server name in the yaml config file
#' @param yamlFile The yaml configuration file with the S3 information
#' @return A list of configuration parameters
GetS3Config <- function(id, yamlFile) {
    
    # Load the configuration
    config <- GetYAMLConfig(id, yamlFile)
    
    # Check if all server information needed is present
    nOfErrors = 0
    
    if(is.null(config$accessKey)) {
        Shout('Access Key not present for ', id)
        nOfErrors = nOfErrors + 1
    }
    
    if(is.null(config$secretKey)) {
        Shout('Secret Key not present for ', id)
        nOfErrors = nOfErrors + 1
    }
    
    if(is.null(config$folder)) {
        Shout('Folder not present for ', id)
        nOfErrors = nOfErrors + 1
    }
    
    # If any error was found stop execution
    if(nOfErrors > 0) {
        stop(paste(nOfErrors, 'error(s) found. Review yaml config file.'))
    }
    
    # Return the config
    return(config)
    
}