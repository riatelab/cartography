#' @name choro
#' @title choro
#' @description add color gradients to spdf according to data classes
#' @param var vector of valuesused
#' @param distr vector of classes
#' @param col vector of colors
#' @param nclass number of classes targeted (if null, the Huntsberger method is used)
#' @param method discretization method ("sd", "equal", "quantile", "jenks","q6","geom")
#' @return List: a vector of colors, colors and distr
#' @noRd
choro <- function(var, distr = NULL, col = NULL, nclass = NULL,
                  method="quantile")
{
  # Discretization
  if (is.null(distr)){
    distr <- discretization(v = var, nclass = nclass, method = method)
  }
  # Colors
  if(is.null(col)){
    col <- carto.pal(pal1 = "blue.pal",n1 = (length(distr) - 1))
  }
  
  # Affectation des couleurs au spdf
  colMap <- col[findInterval(var,distr,all.inside=TRUE)]
  colMap[is.na(colMap)] <- NA

  return(list(colMap = colMap, distr = distr, col = col))
}
