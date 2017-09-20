#' @export
#' @name parallelMeanBoot
#' @author João Monteiro
#' @title Get Mean of vector by bootstrap
#' @description Parallelized implementation of the smean.cl.boot function from the Hmisc package
#' @param x numerical vector
#' @param conf.int confidence interval for the estimation of the mean
#' @param B number of resamplings for the bootstrapp process
#' @param na.rm removes NAs from x. Defaults to TRUE
#' @param reps if TRUE the bootstrapped means will be returned as the reps attribute of the returned object
#' @return vector of the mean, and upper and lower estimates
parallelMeanBoot <- function (x, 
                              conf.int = 0.95, 
                              B = 1000, 
                              na.rm = TRUE, 
                              reps = FALSE) {
    
    if (na.rm) 
        x <- x[!is.na(x)]
    
    n <- length(x)
    xbar <- mean(x)
    
    if (n < 2L) 
        return(c(Mean = xbar, Lower = NA, Upper = NA))
    
    # detect number of cores
    numCores <- parallel::detectCores()
    
    # make a cluster equal to the number of cores
    clusterForPar <- parallel::makeCluster(numCores - 1)
    
    # bootstrapped means
    z <- unlist(parallel::parLapply(
        clusterForPar,
        seq_len(B),
        fun = function(i, vec, N) {
            sum(vec[sample.int(N, N, TRUE, NULL)])
        }, vec = x, N = n
    )) / n
    
    # stop the cluster
    parallel::stopCluster(clusterForPar)
    
    # cut off at choosen confidence interval
    quant <- quantile(z, c((1 - conf.int)/2, (1 + conf.int)/2))
    names(quant) <- NULL
    
    # produce vector of summary statistics
    res <- c(Mean = xbar, Lower = quant[1L], Upper = quant[2L])
    
    # store bootstrapped means
    if (reps) 
        attr(res, "reps") <- z
    res
}
