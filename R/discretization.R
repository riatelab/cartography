#' @title Discretization
#' @name discretization
#' @description Discretization function.
#' @param v a vector of numeric values.
#' @param nclass a number of classes
#' @param method a discretization method; one of "sd", "equal", 
#' "quantile", "jenks","q6" or "geom"  (see Details).
#' @details 
#' "sd", "equal", "quantile" and "jenks" are \link{classIntervals} methods. The "q6" method
#' uses the following \link{quantile} probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.   
#' The "geom" method is based on a geometric progression along the variable values.  
#' @note This function is mainly a wrapper around classIntervals function of 
#' the classInt package + q6 and geom methods. 
#' @examples
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
#' breaks <- discretization(v = var, nclass = 6, method = "quantile")
#' hist(var, probability = TRUE, breaks = breaks, col = "#F0D9F9")
#' rug(var)
#' moy <- mean(var)
#' med <- median(var)
#' abline(v = moy, col = "red", lwd = 3)
#' abline(v = med, col = "blue", lwd = 3)
#' 
#' # Geometric intervals
#' breaks <- discretization(v = var, nclass = 8, method = "geom")
#' hist(var, probability = TRUE, breaks = breaks, col = "#F0D9F9")
#' rug(var)
#' moy <- mean(var)
#' med <- median(var)
#' abline(v = moy, col = "red", lwd = 3)
#' abline(v = med, col = "blue", lwd = 3)
#' @return A numeric vector of breaks
#' @import classInt
#' @import stats
#' @export
discretization <- function(v, nclass = NULL, method = "quantile"){
  v <- as.vector(na.omit(v))
  classIntMethods <- c("sd", "equal", "quantile", "jenks")
  if(is.null(nclass)){
    nclass <- round(1+3.3*log10(length(v)),0)
  }
  if (method %in% classIntMethods){
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
  }
  return(intervals)
}

