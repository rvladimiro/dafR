#' @export
#' @name GetBootstrappedCI
#' @title Constructs Confidence Interval for difference in means
#' @author João Monteiro
#' @description Receives a dataset with a response column, containing the measure for which the confidence interval will be constructed.
#' The confidence interval will always refer to the difference in means between a treatment gorup and the control group.
#' It is expected for the dataset to contain one measure and group assignment per observation
#' 
#' This function should also receive a character vector that identifies the name of the control group, as well as a vector of strings containing the names of the treatment groups to compare against the control
#' The Name of the column containing the measure for the confidence interval should also be provided
#' 
#' Each bootstrap is performed via the smean.cl.boot on the Hmisc package.
#' 
#' Returns the calculated statistics for each bootstrap iteration as well as the boundaries for the confidence interval
#' @param dataset The dataset containing one observation per row
#' @param testGroups A character vector containing the names of the treatement groups
#' @param controlGroup The name which identifies the control group in the dataset
#' @param responseColumn The name of the column that contains the measure to be tested
#' @param groupColumn The name of the column that contains the group identification
#' @param bootIterations The number of bootstrap resamples to perform
#' @param bootConfLevel The confidence level for the interval. Between 1 and 100
#' @return List containing two dataframes. The first dataframe contains the boundaries of the CI. The second dataframe contains all calculated estimates for the mean of each group
GetBootstrappedCI <- function(dataset,
                              testGroups,
                              controlGroup = 'Control',
                              responseColumn = 'response',
                              groupColumn = 'abGroup',
                              bootIterations = 999,
                              bootConfLevel = 95) {
    
    
    # Make sure dataset is a data table
    dataset <- data.table::as.data.table(dataset)
    
    # Make sure it contains a column called response
    if (!(responseColumn %in% names(dataset)))
        stop(paste(
            'dataset must contain a column called', 
            responseColumn,
            'with the test measures'
        ))
    
    
    # Make sure Confidence level makes sense and extract quantiles cuts
    if (!(bootConfLevel > 0 & bootConfLevel < 100))
        stop('confidence level must be between 0 and 100')
    
    excludeQuantity <- 1 - (bootConfLevel / 100)
    quantileCut <- c(0 + excludeQuantity/2, 1 - excludeQuantity/2)
    
    
    # Get control bootrapped estimate for response variable --------------------
    dataset[[groupColumn]] <- as.character(dataset[[groupColumn]])
    
    
    cat('Bootstrapped estimates for', controlGroup, '\n')
    
    controlResponse <- dataset[
        get(groupColumn) == controlGroup, get(responseColumn)
    ]
    
    controlBootEstimate <- attr(Hmisc::smean.cl.boot(
        controlResponse, B = bootIterations, reps = TRUE
    ), 'reps')
    
    
    # Initialise results 
    bootResultsCI <- data.table::data.table()
    bootResultsEstimate <- data.table::data.table(
        abGroup = controlGroup,
        estimates = controlBootEstimate
    )
    
    
    
    # Compare to each test group
    for (gr in testGroups) {
        
        cat('Bootstrapped estimates for', gr, '\n')
        
        
        # Get test group bootrapped estimate for response variable
        testResponse <- dataset[get(groupColumn) == gr, get(responseColumn)]
        testBootEstimate <- attr(Hmisc::smean.cl.boot(
            testResponse, B = bootIterations, reps = TRUE
        ), 'reps')
        
        # get quantile values from difference in estimates to get CI
        lowHighCI <- quantile(
            testBootEstimate - controlBootEstimate,
            probs = quantileCut
        )
        
        # Add measures to results dataset
        bootResultsCI <- rbind(
            bootResultsCI,
            data.table::data.table(
                abGroup = gr,
                lowCIvalue = lowHighCI[[1]],
                highCIvalue = lowHighCI[[2]]
            )
        )
        
        bootResultsEstimate <- rbind(
            bootResultsEstimate,
            data.table::data.table(
                abGroup = gr,
                estimates = testBootEstimate
            )
        )
        
    }
    
    
    # Return list with both data sets
    return(list(
        bootConfInt = bootResultsCI, bootEstimates = bootResultsEstimate
    ))
    
}

