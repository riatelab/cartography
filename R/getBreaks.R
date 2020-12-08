#' @title Classification
#' @name getBreaks
#' @description A function to classify continuous variables.
#' @param v a vector of numeric values.
#' @param nclass a number of classes
#' @param method a classification method; one of "fixed", "sd", "equal", "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "q6", "geom", "arith", "em" or "msd" (see Details).
#' @param k number of standard deviation for "msd" method (see Details)..
#' @param middle creation of a central class for "msd" method (see Details). 
#' @param ... further arguments of \code{\link[classInt:classIntervals]{classIntervals}}.
#' @seealso \link[classInt:classIntervals]{classIntervals} \link{tc_get_breaks}
#' @keywords internal
#' @details 
#' "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust",
#' "bclust", "fisher", "jenks" and "dpih" are \code{\link[classInt:classIntervals]{classIntervals}}
#' methods. You may need to pass additional arguments for some of them.\cr\cr
#' Jenks ("jenks" method) and Fisher-Jenks ("fisher" method) algorithms are based on the same principle and give 
#' quite similar results but Fisher-Jenks is much faster. \cr\cr
#' The "q6" method uses the following \code{\link[stats:quantile]{quantile}} probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.\cr\cr   
#' The "geom" method is based on a geometric progression along the variable values.\cr\cr
#' The "arith" method is based on an arithmetic progression along the variable values.\cr\cr
#' The "em" method is based on nested averages computation.\cr\cr 
#' The "msd" method is based on the mean and the standard deviation of a numeric vector. 
#' The \code{nclass} parameter is not relevant, use \code{k} and \code{middle} instead. \code{k} indicates 
#' the extent of each class in share of standard deviation. If \code{middle=TRUE} then 
#' the mean value is the center of a class else the mean is a break value. 
#' @note This function is mainly a wrapper of \code{\link[classInt:classIntervals]{classIntervals}} + 
#' "arith", "em", "q6", "geom" and "msd" methods. 
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' var <- mtq$MED
#' # Histogram
#' hist(var, probability = TRUE, breaks = 20)
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
#' # Pretty breaks
#' breaks <- getBreaks(v = var, nclass = 4, method = "pretty")
#' hist(var, probability = TRUE, breaks = breaks, col = "#F0D9F9", axes = FALSE)
#' rug(var)
#' axis(1, at = breaks)
#' axis(2)
#' abline(v = med, col = "blue", lwd = 6)
#' 
#' # kmeans method
#' breaks <- getBreaks(v = var, nclass = 4, method = "kmeans")
#' hist(var, probability = TRUE, breaks = breaks, col = "#F0D9F9")
#' rug(var)
#' abline(v = med, col = "blue", lwd = 6)
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
#' @export
getBreaks <- function(v, nclass = NULL, method = "quantile", 
                      k = 1, middle = FALSE, ...){
  
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::getBreaks()",
                            with = "cartography::tc_get_breaks()")  
  
  v <- as.vector(na.omit(v))
  customMethods <- c("geom", "arith", "q6", "em", "msd")
  
  if(is.null(nclass)){
    nclass <- round(1+3.3*log10(length(v)),0)
  }
  if (!method %in% customMethods){
    if (method=="fisher-jenks"){method <- "fisher"} # Backwards compatibility
    intervals <- classInt::classIntervals(v,nclass,style=method, ...)$brks
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
    if (method=="arith")
    {
      intervals <- min(v)
      intervals <- c(intervals,max(v))
      r <- (max(v) - min(v)) / sum(1:nclass) # raison
      tmp <- min(v)
      for ( i in 1:(nclass-1)) {
        intervals <- c(intervals,tmp+r)
        tmp <- tmp+r
        intervals <- sort(intervals)
      }
    }
    if (method == "q6")
    {
      intervals <- as.vector(quantile(v, probs = 
                                        c(0, 5, 27.5, 50, 72.5, 95, 100)/100))
    }
    if (method == "em"){
      t <- bitwAnd(nclass,(nclass-1))
      if(t != 0){
        stop("The number of classes must be a power of 2")
      }else{
        minVec <- min(v)
        maxVec <- max(v)
        it <- log2(nclass)
        
        int <- c(minVec,maxVec)
        
        for (a in 1:it){
          valprv <- c()
          for (i in 1:(length(int)-1)){
            if (i == 1){
              subVec <- v[v >= int[i] & v <= int[i+1]]
            }else{
              subVec <- v[v > int[i] & v <= int[i+1]]
            }
            valprv <- c(valprv, mean(subVec))
          }
          int <- c(int, valprv)
          int <-int[order(int)]
        }
        intervals <- int
      }
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
        
        bks <- c((avgVec - 0.5 * sdVec * k) - (1:nege) * (sdVec * k), 
                 (avgVec - 0.5 * sdVec * k), 
                 (avgVec + 0.5 * sdVec * k), 
                 (avgVec + 0.5 * sdVec * k) + (1:pose) * (sdVec * k))
        intervals <- c(minVec, bks[bks> minVec & bks <maxVec], maxVec)
      }
      intervals <- intervals[order(intervals)]
    }
  }
  return(intervals)
}
