#' @name choro
#' @title choro
#' @description add color gradients to spdf according to data classes
#' @param var vector of valuesused
#' @param distr vector of classes
#' @param col vector of colors
#' @param nclass number of classes targeted (if null, the Huntsberger method is used)
#' @param method discretization method ("sd", "equal", "quantile", "fisher-jenks","q6","geom")
#' @return List: a vector of colors, colors and distr
#' @noRd
choro <- function(var, distr = NULL, col = NULL, nclass = NULL,
                  method="quantile")
{
  # Discretization
  if (is.null(distr)){
    distr <- getBreaks(v = var, nclass = nclass, method = method)
    
    # Colors
    if(is.null(col)){
      col <- carto.pal(pal1 = "blue.pal",n1 = (length(distr) - 1))
    }
    
    colMap <- col[findInterval(var,distr,all.inside=TRUE)]
  }else{
    inter <-findInterval(var,distr,all.inside=FALSE, rightmost.closed	=T)
    inter[inter==0] <- length(distr)
    if(is.null(col)){
      col <- carto.pal(pal1 = "blue.pal",n1 = (length(distr) - 1))
    }
    colMap <- col[inter]
  }
  
  
  
  # Affectation des couleurs au spdf
  # colMap <- col[findInterval(var,distr,all.inside=TRUE)]
  # colMap[is.na(colMap)] <- NA

  return(list(colMap = colMap, distr = distr, col = col))
}
