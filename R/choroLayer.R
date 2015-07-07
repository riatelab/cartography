#' @title Choropleth Layer
#' @name choroLayer
#' @description Plot a chorpoleth layer.
#' @param spdf SpatialPolygonsDataFrame.
#' @param df data.frame; df contains the values to plot.
#' @param spdfid character; id field in spdf, default to the first column 
#' of the spdf data.frame. (optional)
#' @param dfid character; id field in df, default to the first column 
#' of df. (optional)
#' @param var character; name of the numeric field in df to plot.
#' @param breaks numeric; break points in sorted order to indicate the intervals for assigning the colors. 
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) breakpoints (see Details).
#' @param col character; a vector of colors. Note that if breaks is specified there must be one less 
#' colors specified than the number of break. 
#' @param nclass numeric; a targeted number of classes. If null, the number of class is automatically defined (see Details).
#' @param method character; a discretization method; one of "sd", "equal", 
#' "quantile", "jenks","q6","geom"  (see Details).
#' @param border character; color of polygon borders.
#' @param lwd numeric; borders width.
#' @param legend.pos character; position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt character; title of the legend.
#' @param legend.title.cex numeric; size of the legend title.
#' @param legend.values.cex numeric; size of the values in the legend.
#' @param legend.values.rnd numeric; number of decimal places of the values in 
#' the legend.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.nodata character; no data label.
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details 
#' The optimum number of class depends on the number of geographical objects. If nclass is not defined 
#' an automatic method inspired by Sturges (1926) is used : nclass = 1+3.3*log10(N), where nclass is the number 
#' of class and N is the variable length.
#' 
#' 
#' If breaks is used then nclass and method are not.
#' 
#' "sd", "equal", "quantile" and "jenks" are \link{classIntervals} methods. The "q6" method
#' uses the following \link{quantile} probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.   
#' The "geom" method consists is based on a geometric progression along the variable values.  
#' @references Herbert A. Sturges, «
#' \emph{The Choice of a Class Interval }», Journal of the American Statistical Association, vol. 21, n° 153, mars 1926, p. 65-66.
#' @return A plot is returned
#' @export
#' @examples
#' ## Exemple 1
#' data("nuts2006")
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
#'            legend.title.txt = "unemployement rate\n(%)",
#'            legend.values.rnd = 1)
#' 
#' ## Exemple 2
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
                       breaks = NULL, col = NULL, nclass = NULL, 
                       method = "quantile",
                       border = NA, lwd = 1,
                       legend.pos = "bottomleft", 
                       legend.title.txt = var,
                       legend.title.cex = 0.8, 
                       legend.values.cex = 0.6,
                       legend.frame = FALSE,
                       legend.values.rnd = 0,
                       legend.nodata = "no data",
                       add = TRUE)
{
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  # Join
  spdf@data <- data.frame(spdf@data, df[match(spdf@data[,spdfid], df[,dfid]),])
  
  # get the colors and breaks
  layer <- choro(var=spdf@data[,var], distr = breaks, col = col,
                 nbclass = nclass, method = method)
  # poly
  plot(spdf, col = as.vector(layer$colMap), border = border, lwd = lwd, 
       add = add)

  nodata <- FALSE
  if(max(is.na(df[,var])>0)){nodata <- TRUE}
  
  if(legend.pos !="n"){
    LegendChoro(pos = legend.pos, 
                legTitle = legend.title.txt,
                legTitleCex = legend.title.cex,
                legValuesCex = legend.values.cex,
                distr = layer$distr, 
                cols = layer$col, 
                round = legend.values.rnd,
                frame = legend.frame, 
                symbol="box", 
                nodata = nodata, 
                nodatalabel = legend.nodata)
    
  }
  
}
