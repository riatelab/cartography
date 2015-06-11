#' discretization
#'
#' discretization function
#' @details bla bla bla
#'
#' @param v vector
#' @param nbclass number of classes
#' @param method methods of discretisation : "sd", "equal", "quantile", "jenks", "geom", "q6"
#'
#' @examples
#'
#' # 1) SELECT AND VISUALIZE AN INDICATOR
#'
#' myfield<-TNdeleg$housing
#'
#' hist(myfield, probability=TRUE, nclass=30)
#' rug(myfield)
#' moy <- mean(myfield)
#' med <- median(myfield)
#' abline(v=moy,col="red",lwd=3)
#' abline(v=med,col="blue",lwd=3)
#'
#' # 2) CHOOSE A METHOD
#'
#' # Amplitude égale
#' bornes <- discretization(v=myfield, nbclass=6, method="equal")
#' round(bornes, 0)
#'
#' # Progression géométrique
#' bornes <- discretization(v=myfield, nbclass=8, method="geom")
#' round(bornes, 0)
#'
#' # 3) PLOT THE CLASSES
#'
#' # Histogramme des classes
#' #hist(myfield, probability=TRUE, breaks=bornes,col="#F0D9F9")
#' #rug(myfield)
#' #moy <- mean(myfield)
#' #med <- median(myfield)
#' #abline(v=moy,col="red",lwd=3)
#' #abline(v=med,col="blue",lwd=3)
#'
#' @return numeric
#' @import classInt
#' @export

discretization <- function(v,nbclass=NULL,method="quantile"){

  v <- as.vector(na.omit(v))

  classIntMethods <- c("sd", "equal", "quantile", "jenks")


  if(is.null(nbclass)){
    nbclass<- round(1+3.3*log10(length(v)),0)
  }


  if (method %in% classIntMethods){
    intervals <- classInt::classIntervals(v,nbclass,style=method)$brks

  } else {


    if (method=="geom")
    {


      intervals<-min(v)
      intervals<-c(intervals,max(v))
      r<-exp((log(max(v))-log(min(v)))/nbclass) # raison
      tmp<-min(v)
      for ( i in 1:(nbclass-1)) {
        intervals<-c(intervals,tmp*r)
        tmp<-tmp*r
        intervals<-sort(intervals)
      }
    }


    if (method=="q6")
    {
      intervals<-as.vector(quantile(v,probs = c(0, 5, 27.5, 55, 72.5, 95, 100)/100))
    }

}

  return(intervals)
}

