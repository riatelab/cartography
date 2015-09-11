#' @title Topology Layer
#' @name typoLayer
#' @description Plot a typology layer.
#' @param spdf a SpatialPolygonsDataFrame.
#' @param df a data frame that contains the values to plot.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the field in df to plot.
#' @param col a vector of colors.
#' @param border color of the polygons borders.
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
#' @seealso \link{propSymbolsTypoLayer}, \link{typoLayer}, \link{legendTypo}
#' @export
#' @examples
#' data(nuts2006)
#' ## Example 1
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' colours <- c("red","green","blue","yellow")
#' typoLayer(spdf = nuts0.spdf, df = nuts0.df, var = "typo")
#' 
#' 
#' ## Example 2
#' # Layout plot
#' layoutLayer(title = "Colors in Europe",
#'             sources = "UMS RIATE, 2015",
#'             scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white",
#'             bg = "#D9F5FF",
#'             extent = nuts0.spdf)
#' #Countries plot
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' typoLayer(spdf = nuts0.spdf, df = nuts0.df,
#'           var="typo", 
#'           legend.pos = "topright", 
#'           legend.title.txt = "Category", 
#'           add=TRUE)
typoLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var, 
                      col = NULL, border = "grey20", lwd = 1,
                      legend.pos = "bottomleft", 
                      legend.title.txt = var,
                      legend.title.cex = 0.8, 
                      legend.values.cex = 0.6,
                      legend.nodata = "no data",
                      legend.frame = FALSE,
                      add = FALSE)
{
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  # Join
  spdf@data <- data.frame(spdf@data[,spdfid], df[match(spdf@data[,spdfid], df[,dfid]),])
  
  # get the colors 
  
  spdf@data$col <- as.factor(spdf@data[, var])

  if (!is.null(col)){
    levels(spdf@data$col) <- col
  } else {
    col <- grDevices::rainbow(nlevels(spdf@data$col))
    levels(spdf@data$col) <- col
  }

  # poly
  plot(spdf, col = as.vector(spdf@data$col), border = border, lwd = lwd, add = add)

  # for the legend  
  mycols <- as.character(levels(spdf@data$col))
  rVal <- as.character(levels(as.factor(spdf@data[, var])))
  
  nodata <- FALSE
  if(max(is.na(df[,var])>0)){nodata <- TRUE}
  
  if(legend.pos !="n"){
    legendTypo(pos = legend.pos, title.txt = legend.title.txt,
               title.cex = legend.title.cex, values.cex = legend.values.cex,
               categ = rVal, 
               col = mycols, 
               frame = legend.frame, 
               symbol="box", 
               nodata = nodata, 
               nodata.txt = legend.nodata)
    
  }
  
  
}
