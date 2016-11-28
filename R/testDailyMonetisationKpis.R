#' @export
#' @name PerformDailyMonetisationTests
#' @title Compares treatment to control on ARPDAU, ARPPU and Conversion Rate
#' @author João Monteiro
#' @description Receives a dataset with a response column. It is expected for this dataset to contain one row per DAU, including non-paying users.
#' The function provides one of three options for measures to test and construct confidence intervals:
#' arpdau: this will use the entirety of the dataset
#' arppu: this will subset the datset to users whose revenue > 0
#' conversion: this will create a biinary column, on which 1 identifies a user who did a purchase
#' 
#' This function should also receive a character vector that identifies the name of the control group, as well as the name of the column that contains the revenue for each DAU
#' 
#' This functions combines the functions PerformPermutation and GetBootstrappedCI to test the most common monetisation KPIs against all avaliable treatement groups
#' 
#' Returns all the p-values and confidence interval boundaries for each treatement. It also returns the calculated statistics for each bootstrap iteration
#' @param dataset The dataset containing one observation per row
#' @param kpi One of the following: 'arpdau', 'arppu', 'conversion'
#' @param revenueColumn The name of the column that contains the measures on revenue per observation
#' @param groupColumn The name of the column that contains the group identification
#' @param controlName The name which identifies the control group in the dataset
#' @param bootIterations The number of bootstrap resamples to perform
#' @param bootConfLevel The confidence level for the interval. Between 1 and 100
#' @param ... Additional arguments to pass on to permTS function
#' @return List containing two dataframes. The first dataset contains all the p-values and CI boundaries. The second dataframe contains all calculated estimates for the mean of each group
PerformDailyMonetisationTests <- function(dataset, 
                                          kpi,
                                          revenueColumn = 'revenue',
                                          groupColumn = 'abGroup',
                                          controlName = 'Control',
                                          bootIterations = 999, 
                                          bootConfLevel = 95, ...) {
    
    
    # Make sure dataset is a data table
    dataset <- data.table::as.data.table(dataset)
    
    # prepare data to permute based on kpi -------------------------------------
    switch (kpi,
            arpdau = {
                dataset$response <- dataset[[revenueColumn]]
            },
            arppu = {
                dataset <- dataset[get(revenueColumn) > 0]
                dataset$response <- dataset[[revenueColumn]]
            },
            conversion = {
                dataset$response <- as.numeric(dataset[[revenueColumn]] > 0)
            },
            stop('KPI can be "arpdau", "arppu" or "conversion"')
    )
    
    
    # Start Tests --------------------------------------------------------------
    # Groups to compare
    groups <- base::setdiff(
        unique(dataset[[groupColumn]]),
        controlName
    )
    
    
    # Permutation tests
    cat('Performing permutation tests for', toupper(kpi), '\n')
    
    permutationTestResults <- PerformPermutationTest(
        dataset = dataset, testGroups = groups, controlGroup = controlName,
        groupColumn = groupColumn, ...
    )
    
    
    # Bootstrap CI
    cat('Getting bootstrapped CI for', toupper(kpi), '\n')
    
    bootstrapEstimatesResults <- GetBootstrappedCI(
        dataset = dataset, testGroups = groups, controlGroup = controlName,
        groupColumn = groupColumn,
        bootIterations = bootIterations, bootConfLevel = bootConfLevel
    )
    
    
    # Join Results
    testResults <- merge(
        permutationTestResults,
        bootstrapEstimatesResults$bootConfInt,
        by = 'abGroup'
    )
    
    bootEstimates <- bootstrapEstimatesResults$bootEstimates
    
    # Add proper labelling to correctly identify KPI
    testResults <- cbind(
        testResults, kpi = kpi
    )
    
    setnames(
        bootEstimates,
        old = 'estimates', new = kpi
    )
    
    
    # return as list with two dataset
    return(list(
        testResults = testResults, bootEstimates = bootEstimates
    ))
    
}
