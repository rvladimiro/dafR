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
                        yamlConfig = '../db.yml',
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
                paste0('\'', filePrefix, '%\'')
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
    nOfFiles <- length(s3FileList)
    
    Say("Copying files from s3")
    
    # Read all objects from s3 by using path on file list
    if (parallel) {
        
        # detect number of cores
        numCores <- parallel::detectCores()
        
        # make a cluster equal to the number of cores
        clusterForPar <- parallel::makeCluster(numCores)
        
        s3Objects <- parallel::parLapply(clusterForPar,
            seq_len(nOfFiles),
            fun = function(i) {
                Windmill("Copying file", i, "of", nOfFiles)
                aws.s3::get_object(s3FileList[i], accelerate = acceleration)
            }
        )
        
        # stop the cluster
        parallel::stopCluster(clusterForPar)
        
    } else {
        
        s3Objects <- lapply(
            seq_len(nOfFiles),
            FUN = function(i) {
                Windmill("Copying file", i, "of", nOfFiles)
                aws.s3::get_object(s3FileList[i], accelerate = acceleration)
            }
        )
        
    }
    
    
    # Exlude all objects with 0 bites, i.e, length 0
    s3Objects <- s3Objects[sapply(s3Objects, FUN = function(x) length(x) > 0)]
    
    nOfObjs <- length(s3Objects)
    
    
    # Read objects as data.frame
    Say("Reading files as data.table:")
    if (parallel) {
        
        # make a cluster equal to the number of cores
        clusterForPar <- parallel::makeCluster(numCores)
        
        s3DataFrames <- suppressWarnings(parallel::parLapply(
            clusterForPar,
            seq_len(nOfObjs),
            fun = function(n) {
                Windmill("Reading", n, "of", nOfObjs)
                iotools::read.delim.raw(
                    rawConnection(s3Objects[[n]]),
                    sep = ';', header = FALSE
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
                iotools::read.delim.raw(
                    rawConnection(s3Objects[[n]]),
                    sep = ';', header = FALSE
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
    
    
    # Finally delete files from s3
    Say("Removing files from s3")
    if (parallel) {
        
        # make a cluster equal to the number of cores
        clusterForPar <- parallel::makeCluster(numCores)
        
        delRes <- parallel::parSapply(
            clusterForPar,
            seq_len(nOfFiles),
            FUN = function(i) {
                Windmill("Deleting file", i, "of", nOfFiles)
                aws.s3::delete_object(s3FileList[i], accelerate = acceleration)
            }
        )
        
        # Stop the cluster
        parallel::stopCluster(clusterForPar)
        
    } else {
        
        delRes <- sapply(
            seq_len(nOfFiles),
            FUN = function(i) {
                Windmill("Deleting file", i, "of", nOfFiles)
                aws.s3::delete_object(s3FileList[i], accelerate = acceleration)
            }
        )
        
    }
    
    
    return(dt)
    
}
