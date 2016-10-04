#' @title Discretization
#' @name getBreaks
#' @description A function to discretize continious variables.
#' @param v a vector of numeric values.
#' @param nclass a number of classes
#' @param method a discretization method; one of "sd", "equal", 
#' "quantile", "fisher-jenks","q6", "geom"  or "msd" (see Details).
#' @param k number of standard deviation for "msd" method (see Details)..
#' @param middle creation of a central class for "msd" method (see Details). 
#' @details 
#' "sd", "equal", "quantile" and "fisher-jenks" are \link{classIntervals} methods.\cr\cr
#' Jenks and Fisher-Jenks algorithms are based on the same principle and give 
#' quite similar results but Fisher-Jenks is much faster. \cr\cr
#' The "q6" method uses the following \link{quantile} probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.\cr\cr   
#' The "geom" method is based on a geometric progression along the variable values.\cr\cr
#' The "msd" method is based on the mean and the standard deviation of a numeric vector. 
#' The nclass parameter is not relevant, use k and middle instead. k indicates 
#' the extent of each class in share of standard deviation. If middle=TRUE then 
#' the mean value is the center of a class else the mean is a break value. 
#' @note This function is mainly a wrapper around classIntervals function of 
#' the classInt package + q6, geom and msd methods. 
#' @examples
#' data("nuts2006")
#' # Create the natality rate
#' var <- nuts2.df$birth_2008/nuts2.df$pop2008 * 1000
#' 
#' # Histogram
#' hist(var, probability = TRUE, nclass = 30)
#' rug(var)
#' moy <- mean(var)
#' med <- median(var)
#' abline(v = moy, col = "red", lwd = 3)
#' abline(v = med, col = "blue", lwd = 3)
#' 
#' # Quantile intervals
#' breaks <- getBreaks(v = var, nclass = 6, method = "quantile")
#' hist(var, probability = TRUE, breaks = breaks, col = "#F0D9F9")
#' rug(var)
#' med <- median(var)
#' abline(v = med, col = "blue", lwd = 3)
#' 
#' # Geometric intervals
#' breaks <- getBreaks(v = var, nclass = 8, method = "geom")
#' hist(var, probability = TRUE, breaks = breaks, col = "#F0D9F9")
#' rug(var)
#' 
#' # Mean and standard deviation (msd)
#' breaks <- getBreaks(v = var, method = "msd", k = 1, middle = TRUE)
#' hist(var, probability = TRUE, breaks = breaks, col = "#F0D9F9")
#' rug(var)
#' moy <- mean(var)
#' sd <- sd(var)
#' abline(v = moy, col = "red", lwd = 3)
#' abline(v = moy + 0.5 * sd, col = "blue", lwd = 3)
#' abline(v = moy - 0.5 * sd, col = "blue", lwd = 3)
#' @return A numeric vector of breaks
#' @import classInt
#' @import stats
#' @export
getBreaks <- function(v, nclass = NULL, method = "quantile", 
                      k = 1, middle = FALSE){
  v <- as.vector(na.omit(v))
  classIntMethods <- c("sd", "equal", "quantile", "fisher-jenks")
  
  if(is.null(nclass)){
    nclass <- round(1+3.3*log10(length(v)),0)
  }
  if (method %in% classIntMethods){
    if (method=="fisher-jenks"){method <- "fisher"}
    intervals <- classInt::classIntervals(v,nclass,style=method)$brks
    
  } else {
    if (method=="geom")
    {
      intervals <- min(v)
      intervals <- c(intervals,max(v))
      r <- exp((log(max(v))-log(min(v)))/nclass) # raison
      tmp <- min(v)
      for ( i in 1:(nclass-1)) {
        intervals <- c(intervals,tmp*r)
        tmp <- tmp*r
        intervals <- sort(intervals)
      }
    }
    if (method == "q6")
    {
      intervals <- as.vector(quantile(v,probs = c(0, 5, 27.5, 50, 72.5, 95, 100)/100))
    }
    if (method == "msd"){

      minVec <- min(v)
      maxVec <- max(v)
      avgVec <- mean(v)
      sdVec <- sqrt(sum((v - avgVec) ^ 2)  / length(v))
      
      if (middle == FALSE){
        pose <- ceiling((maxVec - avgVec) / (sdVec * k))
        nege <- ceiling((avgVec - minVec) / (sdVec *k))
        
        avgVec + (1:pose) * (sdVec * k)
        bks <- c(avgVec - (1:nege) * (sdVec * k), 
                 avgVec, 
                 avgVec + (1:pose) * (sdVec * k) )
        intervals <- c(minVec, bks[bks> minVec & bks <maxVec], maxVec)
      }else{
        pose <- ceiling((maxVec - (avgVec + 0.5 * sdVec * k)) / (sdVec * k))
        nege <- ceiling(((avgVec - 0.5 * sdVec * k ) - minVec) / (sdVec *k))
        
        bks <- c((avgVec -  0.5 * sdVec * k) - (1:nege) * (sdVec * k), 
                 (avgVec -  0.5 * sdVec * k), 
                 (avgVec +  0.5 * sdVec * k), 
                 (avgVec + 0.5 * sdVec * k) + (1:pose) * (sdVec * k))
        intervals <- c(minVec, bks[bks> minVec & bks <maxVec], maxVec)
      }
      intervals <- intervals[order(intervals)]
    }
  }
  return(intervals)
}

