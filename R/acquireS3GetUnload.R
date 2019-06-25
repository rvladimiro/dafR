#' @export
#' @name S3GetUnload
#' @title Read s3 files generated from Unload SQL command
#' @author João Monteiro
#' @description This function reads into a data table the files generated from an unload command.
#' The files that are read in, are the ones that share the prefix passed on the argument
#' The prefix should contain the absolute path to the s3 folder holding the files
#' alternatively we can pass the function a character vector listing all the files to be read
#' 
#' The yaml file must contain s3 bucket name, user and password.
#' @param filePrefix Either a character vector with length 1, with the prefix shared by the objects to be read, or a character vector listing the full path of each obejct to be read
#' @param dbID The name of the yaml group containing the database credentials
#' @param s3ID The name of the yaml group containing the s3 bucket credentials
#' @param yamlConfig The path to the yaml file
#' @param acceleration Use s3 bucket acceleration
#' @param parallel Use parallel package for simultaneous connections to s3
#' @return The function will return a data.table object with the content of the files that were read from the s3 bucket
S3GetUnload <- function(filePrefix, 
                        dbID, 
                        s3ID, 
                        yamlConfig = '~/db.yml',
                        acceleration = TRUE,
                        parallel = TRUE) {
    
    # Get the s3ID configuration
    # read s3 credentials
    s3Config <- GetS3Config(s3ID, yamlConfig)
    
    # set credentials on environment variables for 
    # compaitibility with aws.s3 package
    Sys.setenv(
        "AWS_ACCESS_KEY_ID" = s3Config$accessKey,
        "AWS_SECRET_ACCESS_KEY" = s3Config$secretKey,
        "AWS_DEFAULT_REGION" = ifelse(
            'region' %in% names(s3Config),
            s3Config$region,
            'us-west-2'
        )
    )
    
    # If only one string was passed than it's assumed it is a general prefix
    if (length(filePrefix) == 1) {
        
        # Get the full lits of files from stl_unload_log table
        s3FileList <- PostgreSQLQuery(
            query = paste(
                'SELECT path FROM stl_unload_log',
                'WHERE path LIKE',
                paste0('\'', filePrefix, '%\''),
                'AND line_count > 0'
            ),
            id = dbID,
            yamlFile = yamlConfig,
            printProgress = FALSE
        )
        
        # Confirm that the files exist
        if (nrow(s3FileList) == 0)
            stop('No files with the given prefix were found')
        
        
        # Remove extra spaces
        s3FileList <- gsub(pattern = " ", replacement = "", x = s3FileList$path)
        
    } else {
        s3FileList <- filePrefix   
    }
    
    
    # Read objects from S3
    # number of objects to read and total files
    nOfObjs <- length(s3FileList)

    # Read objects as data.frame
    Say("Reading files from s3 as data.table:")
    if (nOfObjs == 0) {
        
        s3DataFrames <- data.table::data.table()
        
    } else if (parallel) {
        
        # detect number of cores
        numCores <- parallel::detectCores()
        
        # make a cluster equal to the number of cores
        clusterForPar <- parallel::makeCluster(numCores)
        
        s3DataFrames <- suppressWarnings(parallel::parLapply(
            clusterForPar,
            seq_len(nOfObjs),
            fun = function(n) {
                Windmill("Reading", n, "of", nOfObjs)
                aws.s3::s3read_using(
                    object = s3FileList[[n]], 
                    FUN = data.table::fread,
                    header = FALSE,
                    opts = list(accelerate = acceleration)
                )
            }
        ))
        
        # stop the cluster
        parallel::stopCluster(clusterForPar)
        
    } else {
        
        s3DataFrames <- suppressWarnings(lapply(
            seq_len(nOfObjs),
            FUN = function(n) {
                Windmill("Reading", n, "of", nOfObjs)
                aws.s3::s3read_using(
                    object = s3FileList[[n]], 
                    FUN = data.table::fread,
                    header = FALSE,
                    opts = list(accelerate = acceleration)
                )
            }
        ))
        
    }
    
    
    # Close all opened connections
    closeAllConnections()
    
    # Create data.table
    Say("Creating data frame...")
    dt <- data.table::rbindlist(s3DataFrames, fill = TRUE)
    
    
    # Find and fix int64 columns
    int64Columns <- grep("integer64", sapply(dt, class))
    if(length(int64Columns) > 0) {
        for(column in int64Columns) {
            dt[[column]] <- as.integer(dt[[column]])
        }
    }
    
    return(dt)
    
}
