#' @title choroLayer
#' @description plot a chorpoleth layer
#' @name choroLayer
#' @details plot a chorpoleth layer
#' @param spdf spatial dataframe (geometries)
#' @param df dataframe (data)
#' @param spdfid id of the spdf (if null, the first column is used)
#' @param dfid id of the df (if null, the first column is used)
#' @param var field used
#' @param distr vector of classes
#' @param col vector of colors
#' @param nbclass number of classes targeted (if null,
#' the Huntsberger method is used)
#' @param method discretization method ("sd", "equal",
#' "quantile", "jenks","q6","geom")
#' @param add add = T
#' @examples
#' choroLayer(spdf = TNdeleg.spdf, df = TNdeleg, var =  "housing", nbclass = 20,
#' method = "equal",add=FALSE)
#' @return plot
#' @export
choroLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var, distr = NULL,
                       col = NULL, nbclass = NULL, method = "quantile",
                       add = FALSE)
{
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid<-names(df)[1]}

  # Join
  spdf@data <- data.frame(spdf@data, df[match(spdf@data[,spdfid], df[,dfid]),])
# get the colors and distr
  layer <- choro(var=spdf@data[,var], distr = distr, col = col,
                 nbclass = nbclass, method = method)
  # poly
  plot(spdf, col = as.vector(layer$colMap),
       border = "black", lwd = 1, add = add)

  # lines (todo)
  # dots (todo)
}


