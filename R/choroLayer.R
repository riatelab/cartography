#' @title Choropleth Layer
#' @name choroLayer
#' @description Plot a chorpoleth layer.
#' @param spdf Spatial*DataFrame; if \code{spdf} is a SpatialPolygonsDataFrame 
#' symbols are plotted on centroids.
#' @param df data.frame; \code{df} contains the values to plot.
#' @param spdfid character; id field in \code{spdf}, default to the first column 
#' of the \code{spdf} data.frame. (optional)
#' @param dfid character; id field in \code{df}, default to the first column 
#' of \code{df}. (optional)
#' @param var character; name of the numeric field in \code{df} to plot.
#' @param distr numeric; a vector of breaks. (see Details)
#' @param col character; a vector of colors.
#' @param nbclass numeric; a targeted number of classes (if null,the Huntsberger 
#' method is used).
#' @param method character; a discretization method; one of "sd", "equal", 
#' "quantile", "jenks","q6","geom"  (see Details)
#' @details If \code{distr} is used, \code{nbclass} and \code{method} are not.
#' 
#' "sd", "equal", "quantile" and "jenks" are \link{classIntervals} methods. "q6" 
#' is ... and "geom" is ...
#' @param legend.pos character; position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param legend.title.txt character; title of the legend.
#' @param legend.title.cex numeric; size of the legend title.
#' @param legend.values.cex numeric; size of the values in the legend.
#' @param legend.values.rnd numeric; number of decimal places of the values in 
#' the legend.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @return A plot is returned
#' @export
#' @examples
#' data("nuts2006")
#' 
#' # Layout plot
#' layoutLayer(title = "Countries Population in Europe",
#'             sources = "UMS RIATE, 2015",
#'             author = "UMS RIATE",
#'             scale = 0,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white",
#'             bg = "#D9F5FF",
#'             south = TRUE,
#'             extent = nuts0.spdf)
#' 
#' # GDP per capita
#' nuts0.df$gpppercapita <- nuts0.df$gdppps2008 * 1000000 / nuts0.df$pop2008
#' 
#' # Plot the GDP per capit
#' choroLayer(spdf = nuts0.spdf, 
#'            df = nuts0.df,
#'            var = "gpppercapita", 
#'            legend.pos = "right", 
#'            nbclass = 6, 
#'            col = c("#FEE5D9", "#FCBBA1", "#FC9272", "#FB6A4A" ,"#DE2D26" ,"#A50F15"),
#'            method = "q6", 
#'            legend.values.cex = 0.5, 
#'            add = TRUE,
#'            legend.title.txt = "GDP per capita\n(euros/inhabitants)", 
#'            legend.frame = TRUE,
#'            legend.values.rnd = 0)
choroLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var, 
                       distr = NULL, col = NULL, nbclass = NULL, method = "quantile",
                       legend.pos = "bottomleft", legend.title.txt = var,
                       legend.title.cex = 0.8, legend.values.cex = 0.6,
                       legend.frame = FALSE,
                       legend.values.rnd = 0, add = TRUE)
{
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid<-names(df)[1]}
  
  # Join
  spdf@data <- data.frame(spdf@data, df[match(spdf@data[,spdfid], df[,dfid]),])
  
  # get the colors and distr
  layer <- choro(var=spdf@data[,var], distr = distr, col = col,
                 nbclass = nbclass, method = method)
  # poly
  plot(spdf, col = as.vector(layer$colMap), border = "black", lwd = 1, 
       add = add)
  
  LegendChoro(pos = legend.pos, 
              legTitle = legend.title.txt,
              legTitleCex = legend.title.cex,
              legValuesCex = legend.values.cex,
              distr = layer$distr, 
              cols = layer$col, 
              round = legend.values.rnd,
              frame = legend.frame, 
              symbol="box", 
              nodata = FALSE, 
              nodatalabel = "No data available")
  
}


