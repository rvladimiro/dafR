#' @export
#' @import data.table
#' @name PerformPermutationTest
#' @title Performs a permutation test
#' @author João Monteiro
#' @description Receives a dataset with a response column, containing the measure to be tested
#' It is expected for the dataset to contain one measure and group assignment per observation
#' 
#' This function should also receive a character vector that identifies the name of the control group, as well as a vector of strings containing the names of the treatment groups to compare against the control
#' The Name of the column containing the measure to be tested should also be provided
#' 
#' Each individual permutation test is performed via the permTS function, from the perm package
#' 
#' Returns all p-values and statistics for each comparison to control
#' @param dataset The dataset containing one observation per row
#' @param testGroups A character vector containing the names of the treatement groups
#' @param controlGroup The name which identifies the control group in the dataset
#' @param responseColumn The name of the column that contains the measure to be tested
#' @param groupColumn The name of the column that contains the group identification
#' @param ... Additional arguments to pass on to permTS function
#' @return A dataframe containing p-values and statistics for each comparion to control
PerformPermutationTest <- function(dataset,
                                   testGroups,
                                   controlGroup = 'Control',
                                   responseColumn = 'response',
                                   groupColumn = 'abGroup', ...) {
    
    
    # Make sure dataset is a data table
    dataset <- data.table::as.data.table(dataset)
    
    # Make sure it contains a column called response
    if (!(responseColumn %in% names(dataset)))
        stop(paste(
            'dataset must contain a column called', 
            responseColumn,
            'with the test measures'
        ))
    
    
    
    # initialise results -------------------------------------------------------
    testResults <- data.table::data.table()
    
    # Get control reference values
    dataset[[groupColumn]] <- as.character(dataset[[groupColumn]])
    controlResponse <- dataset[
        get(groupColumn) == controlGroup, get(responseColumn)
    ]
    
    # Perform test for each test group -----------------------------------------
    for (gr in testGroups) {
        
        cat('Permutation Test for', gr, '\n')
        
        # get test group
        testResponse <- dataset[get(groupColumn) == gr, get(responseColumn)]
        
        # Perform permutation test
        permTest <- perm::permTS(
            x = testResponse, y = controlResponse,
            alternative = 'two.sided', ...
        )
        
        # Add relevant measures to results dataset
        testResults <- rbind(
            testResults,
            data.table::data.table(
                abGroup = gr,
                meanControl = mean(controlResponse),
                meanTest = mean(testResponse),
                diffToControl = permTest$estimate,
                pValueLower = permTest$p.values[['p.lte']],
                pValueGreater = permTest$p.values[['p.gte']],
                pValueDiff = permTest$p.values[['p.twosided']]
            )
        )
    }
    
    # retrive results
    return(testResults)
}
