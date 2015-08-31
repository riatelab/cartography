#' @title Topology Layer
#' @name typoLayer
#' @description Plot a typology layer.
#' @param spdf a SpatialPolygonsDataFrame.
#' @param df a data.frame that contains the values to plot.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data.frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the factor field in df to plot.
#' @param col a vector of colors. Note that if breaks is specified there must be one less 
#' colors specified than the number of break. 
#' @param border color of polygon borders.
#' @param lwd borders width.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.nodata no data label.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @references Herbert A. Sturges, «
#' \emph{The Choice of a Class Interval }», Journal of the American Statistical Association, vol. 21, n° 153, mars 1926, p. 65-66.
#' @export
#' @examples
#' data(nuts2006)
#' nuts0.df$typo <- substr(nuts0.df$id,2,2)
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' colours <- c("red","green","blue","yellow")
#' 
#' typoLayer(spdf = nuts0.spdf, df = nuts0.df, var = "typo", col=colours,
#'           legend.pos = "topright", border="white",
#'           legend.title.txt = "Typologie",add=FALSE
#' )


typoLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var, 
                       col = NULL, border = NA, lwd = 1,
                       legend.pos = "bottomleft", 
                       legend.title.txt = var,
                       legend.title.cex = 0.8, 
                       legend.values.cex = 0.6,
                       legend.frame = FALSE,
                       legend.nodata = "no data",
                       add = TRUE)
{
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  # Join
  spdf@data <- data.frame(spdf@data, df[match(spdf@data[,spdfid], df[,dfid]),])
  
  # get the colors 
  
  spdf@data$col <- as.factor(spdf@data[, var])
  levels(spdf@data$col) <- col
  mycols <- as.character(levels(spdf@data$col))
  rVal <- as.character(levels(as.factor(spdf@data[, var])))

  # poly
  plot(spdf, col = as.vector(spdf@data$col), border = border, lwd = lwd, add = add)

  nodata <- FALSE
  if(max(is.na(df[,var])>0)){nodata <- TRUE}
  
  if(legend.pos !="n"){
#     LegendTypo(pos = legend.pos, 
#                 legTitle = legend.title.txt,
#                 legTitleCex = legend.title.cex,
#                 legValuesCex = legend.values.cex,
#                 categ = rVal, 
#                 cols = mycols, 
#                 frame = legend.frame, 
#                 symbol="box", 
#                 nodata = nodata, 
#                 nodatalabel = legend.nodata)
#     
  }
  

}
