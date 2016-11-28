#' @export
#' @name PerformCohortRetentionTests
#' @title Compares treatment to control on retention rate for given retention day
#' @author João Monteiro
#' @description This function receives a summarised dataset and compares the retention rates for each avaliable test group against the control.
#' The dataset should have the columns containing the following information:
#' acquisition_date | activity_date | retention_day | group | active_users
#' The names of the columns can differ
#' 
#' This function should also receive a character vector that identifies the name of the control group, as well as the names of the columns containing metrics of interest
#' 
#' This function uses the prop.test function to perform a simple chi^2 on the proportion of users who return N days after their acquisition.
#' If no retention days are provided, then the function will perform tests for each one avaliable.
#' 
#' Returns p-value for two-tailed test and statistics for each comparison to control
#' @param dataset The summarised dataset
#' @param retentionDays An integer vector containing the retention days to test. If not provided will test all avaliable non-zero retention days
#' @param controlName The name which identifies the control group in the dataset
#' @param groupColumn The name of the column that contains the group identification
#' @param retentionDaysColumn The name of the column that contains the retention day identification
#' @param usersColumn The name of the column that contains the sum of users for each combination of acquisition and activity dates
#' @param acquisitionColumn The name of the column that contains the dates of acquisition
#' @param ... Additional arguments to pass on to prop.test function
#' @return A dataframe containing p-value for two-tailed test, boundaries for confidence intervals, for each tested retentention day and treatment group
PerformCohortRetentionTests <- function(dataset, 
                                        retentionDays,
                                        controlName = 'Control',
                                        groupColumn = 'abGroup',
                                        retentionDaysColumn = 'retentionDay',
                                        usersColumn = 'users',
                                        acquisitionColumn = 'cohort', ...) {
    
    
    # Make sure dataset contains required columns
    if (
        prod(
            c(groupColumn, retentionDaysColumn, usersColumn, acquisitionColumn) 
            %in% names(dataset)
        ) == 0
    )
        stop('Missing required columns')
    
    # Make sure it is a data.table and copy it to avoid side-effects
    dataset <- data.table::as.data.table(dataset)
    df <- data.table::copy(dataset)
    
    
    
    # Scrub dataset for count of returns by retention day ----------------------
    retentionData <- df[,
        cohortSize := get(usersColumn)[get(retentionDaysColumn) == 0],
        by = .(get(acquisitionColumn), get(groupColumn))
    ][,
        .(users = sum(get(usersColumn)), cohortSize = sum(cohortSize)),
        by = .(
            retentionDay = get(retentionDaysColumn), 
            abGroup = get(groupColumn)
        )
    ]
    
    
    
    # Select retention days to test 
    # If not specified will test all avaliable
    if (!hasArg(retentionDays))
        retentionDays <- unique(retentionData$retentionDay)
    
    # Remove day 0 and Check if retention days are valid
    retentionDays <- setdiff(retentionDays, 0)
    
    if (sum(retentionDays < 0) != 0 | sum(retentionDays %% 1) != 0)
        stop('Invalid Retention days to test')
    
    
    # Perform tests of proportions ---------------------------------------------
    
    # test groups
    testGroups <- setdiff(
        unique(retentionData$abGroup),
        controlName
    )
    
    # create data set for results
    resLog <- data.table::data.table()
    
    
    # Perform for each retention day
    for (d in retentionDays) {
        
        # Peform for each group
        for (gr in testGroups) {
            
            # count of successes (returned users)
            suc <- c(
                retentionData[abGroup == controlName & retentionDay == d]$users,
                retentionData[abGroup == gr & retentionDay == d]$users
            )
            
            # count of trials (new users)
            trials <- c(
                retentionData[
                    abGroup == controlName & retentionDay == d
                    ]$cohortSize,
                retentionData[
                    abGroup == gr & retentionDay == d
                    ]$cohortSize
            )
            
            # print status
            cat('\r', 'Testing Retention Day', d, ' | group', gr)
            
            # perform test
            testRes <- prop.test(
                x = suc, n = trials, ...
            )
            
            
            # register results
            resLog <- rbind(
                resLog,
                data.table::data.table(
                    abGroup = gr,
                    retentionControl = testRes$estimate[[1]],   # retention for control
                    retentionTest = testRes$estimate[[2]],    # retention for test group
                    diffToControl = testRes$estimate[[2]] - testRes$estimate[[1]],
                    pValueDiff = testRes$p.value,
                    lowCIvalue = testRes$conf.int[1],
                    highCIvalue = testRes$conf.int[2],
                    kpi = paste0('retention D', d)
                )
            )
            
            
            
            
        }
        
    }
    
    
    # return final results
    return(resLog)
    
}