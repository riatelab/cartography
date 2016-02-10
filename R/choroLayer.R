#' @title Choropleth Layer
#' @name choroLayer
#' @description Plot a chorpoleth layer.
#' @param spdf a SpatialPolygonsDataFrame.
#' @param df a data frame that contains the values to plot.
#' @param spdfid name of the identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid name of the identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df to plot.
#' @param breaks break values in sorted order to indicate the intervals for assigning the colors. 
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) break values (see Details).
#' @param col a vector of colors. Note that if breaks is specified there must be one less 
#' colors specified than the number of break. 
#' @param nclass a targeted number of classes. If null, the number of class is automatically defined (see Details).
#' @param method a discretization method; one of "sd", "equal", 
#' "quantile", "jenks","q6" or "geom"  (see Details).
#' @param border color of the polygons borders.
#' @param lwd borders width.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.nodata no data label.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details 
#' The optimum number of class depends on the number of geographical objects. If nclass is not defined, 
#' an automatic method inspired by Sturges (1926) is used : nclass = 1+3.3*log10(N), where nclass is the number 
#' of class and N is the variable length.
#' 
#' 
#' If breaks is used then nclass and method are not.
#' 
#' "sd", "equal", "quantile" and "jenks" are \link{classIntervals} methods. The "q6" method
#' uses the following \link{quantile} probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.   
#' The "geom" method is based on a geometric progression along the variable values.  
#' @references Herbert A. Sturges, «
#' \emph{The Choice of a Class Interval }», Journal of the American Statistical Association, vol. 21, n° 153, mars 1926, p. 65-66.
#' @seealso \link{discretization}, \link{carto.pal},  \link{legendChoro}, \link{propSymbolsChoroLayer}
#' @export
#' @examples
#' data("nuts2006")
#' 
#' ## Example 1
#' nuts2.df$unemprate <- nuts2.df$unemp2008/nuts2.df$act2008*100
#' choroLayer(spdf = nuts2.spdf,
#'            df = nuts2.df,
#'            var = "unemprate")
#' 
#' ## Example 2
#' nuts2.df$unemprate <- nuts2.df$unemp2008/nuts2.df$act2008*100
#' choroLayer(spdf = nuts2.spdf,
#'            df = nuts2.df,
#'            var = "unemprate",
#'            method = "quantile",
#'            nclass = 8,
#'            col = carto.pal(pal1 = "turquoise.pal", n1 = 8),
#'            border = "grey40",
#'            add = FALSE,
#'            legend.pos = "topright",
#'            legend.title.txt = "Unemployement rate\n(%)",
#'            legend.values.rnd = 1)
#' 
#' ## Example 3
#' # Compute the compound annual growth rate
#' nuts2.df$cagr <- (((nuts2.df$pop2008 / nuts2.df$pop1999)^(1/9)) - 1) * 100
#' summary(nuts2.df$cagr)
#' # Plot the compound annual growth rate
#' cols <- carto.pal(pal1 = "blue.pal", n1 = 2, pal2 = "red.pal", n2 = 4)
#' choroLayer(spdf = nuts2.spdf,
#'            df = nuts2.df,
#'            var = "cagr", breaks = c(-2.43,-1,0,0.5,1,2,3.1),
#'            col = cols,
#'            border = "grey40",
#'            add = FALSE,
#'            legend.pos = "topright",
#'            legend.title.txt = "Compound annual\ngrowth rate",
#'            legend.values.rnd = 2)
#' # Layout plot
#' layoutLayer(title = "Demographic Trends",
#'             sources = "Eurostat, 2008",
#'             scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white")
choroLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var, 
                       breaks = NULL, method = "quantile", nclass = NULL,
                       col = NULL,
                       border = "grey20", lwd = 1,
                       legend.pos = "bottomleft", 
                       legend.title.txt = var,
                       legend.title.cex = 0.8, 
                       legend.values.cex = 0.6,
                       legend.values.rnd = 0,
                       legend.nodata = "no data",
                       legend.frame = FALSE,
                       add = FALSE)
{
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  # Join
  spdf@data <- data.frame(spdf@data[,spdfid], df[match(spdf@data[,spdfid], df[,dfid]),])

  spdf <- spdf[!is.na(spdf@data[,dfid]),]
  
  # get the colors and breaks
  layer <- choro(var=spdf@data[,var], distr = breaks, col = col,
                 nclass = nclass, method = method)
  # poly
  plot(spdf, col = as.vector(layer$colMap), border = border, lwd = lwd, 
       add = add)
  
  nodata <- FALSE
  if(max(is.na(df[,var])>0)){nodata <- TRUE}
  
  if(legend.pos !="n"){
    legendChoro(pos = legend.pos, 
                title.txt = legend.title.txt,
                title.cex = legend.title.cex,
                values.cex = legend.values.cex,
                breaks = layer$distr, 
                col = layer$col, 
                values.rnd = legend.values.rnd,
                frame = legend.frame, 
                symbol="box",  nodata.col = NA,
                nodata = nodata, 
                nodata.txt = legend.nodata)
    
  }
  
}
