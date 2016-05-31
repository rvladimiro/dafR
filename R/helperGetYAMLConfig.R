#' @export
#' @name GetYAMLConfig
#' @author Ricardo Vladimiro
#' @title Get a configuration from a yaml configuration file
#' @param id The id in the yaml config file
#' @param yamlFile The yaml configuration file with the configuration
#' @return A list of configuration parameters
GetYAMLConfig <- function(id, yamlFile) {
    
    # Perform basic checking ---------------------------------------------------
    
    if(missing(id) || is.null(id)){
        stop("ID is null or missing.")
    }
    
    if(!file.exists(yamlFile)) {
        stop("YAML file not present.")
    }
    
    # Load the configuration ---------------------------------------------------
    
    suppressWarnings(
        config <- yaml::yaml.load_file(input = yamlFile)[[id]]
    )
    
    # The config will be NULL if the id doesn't exist in the yaml file
    if(is.null(config)) {
        stop(paste("Configuration for", id, "does not exist."))
    }
    
    # Return the configuration -------------------------------------------------
    return(config)
    
}