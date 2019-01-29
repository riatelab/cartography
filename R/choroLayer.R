#' @title Choropleth Layer
#' @name choroLayer
#' @description Plot a choropleth layer.
#' @param x an sf object, a simple feature collection. If x is used then spdf, df, spdfid and dfid are not.  
#' @param spdf a SpatialPolygonsDataFrame.
#' @param df a data frame that contains the values to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid name of the identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid name of the identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in x or df to plot.
#' @param breaks break values in sorted order to indicate the intervals for assigning the colors. 
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) break values (see Details).
#' @param col a vector of colors. Note that if breaks is specified there must be one less 
#' colors specified than the number of break. 
#' @param nclass a targeted number of classes. If null, the number of class is automatically defined (see Details).
#' @param method a classification method; one of "sd", "equal", "quantile", "fisher-jenks","q6", "geom", "arith", 
#' "em" or "msd" (see \link{getBreaks}).
#' @param border color of the polygons borders.
#' @param lwd borders width.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.nodata no data label.
#' @param legend.border color of boxes borders in the legend.
#' @param legend.horiz whether to display the legend horizontally (TRUE) or
#' not (FALSE).
#' @param colNA no data color. 
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details 
#' The optimum number of class depends on the number of geographical objects. 
#' If nclass is not defined, an automatic method inspired by Sturges (1926) 
#' is used : nclass = 1+3.3*log10(N), where nclass is the number 
#' of class and N is the variable length.\cr
#' 
#' 
#' If breaks is used then nclass and method are not. \cr
#' 
#' If breaks is defined as \code{c(2, 5, 10, 15, 20)} intervals will be: [2 - 5[, [5 - 10[, [10 - 15[, [15 - 20].
#' 
#' 
#' @references Herbert A. Sturges, «
#' \emph{The Choice of a Class Interval }», Journal of the American Statistical 
#' Association, vol. 21, n° 153, mars 1926, p. 65-66.
#' @seealso \link{getBreaks}, \link{carto.pal},  \link{legendChoro}, \link{propSymbolsChoroLayer}
#' @export
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' # Population density
#' mtq$POPDENS <- 1e6 * mtq$POP / st_area(x = mtq)
#' 
#' # Default
#' choroLayer(x = mtq, var = "POPDENS")
#' 
#' # With parameters 
#' choroLayer(x = mtq, var = "POPDENS",
#'            method = "quantile", nclass = 5,
#'            col = carto.pal(pal1 = "sand.pal", n1 = 5),
#'            border = "grey40",
#'            legend.pos = "topright", legend.values.rnd = 0,
#'            legend.title.txt = "Population Density\n(people per km2)")
#' 
#' # Layout
#' layoutLayer(title = "Population Distribution in Martinique, 2015",
#'             author = "", sources = "")
choroLayer <- function(x, spdf, df, spdfid = NULL, dfid = NULL, var, 
                       breaks = NULL, method = "quantile", nclass = NULL,
                       col = NULL,
                       border = "grey20", lwd = 1,
                       colNA = "white",
                       legend.pos = "bottomleft", 
                       legend.title.txt = var,
                       legend.title.cex = 0.8, 
                       legend.values.cex = 0.6,
                       legend.values.rnd = 0,
                       legend.nodata = "no data",
                       legend.frame = FALSE,
                       legend.border = "black",
                       legend.horiz = FALSE,
                       add = FALSE){
  
  if (missing(x)){
    x <- convertToSf(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid)
  }
  
  # get the colors and breaks
  layer <- choro(var = x[[var]], distr = breaks, col = col, nclass = nclass, 
                 method = method)
  
  colVec <- as.vector(layer$colMap)
  
  nodata <- FALSE
  if(max(is.na(x[[var]]) > 0)){
    nodata <- TRUE
    colVec[is.na(colVec)] <- colNA
  }
  
  # plot
  if(max(class(sf::st_geometry(x)) %in% c("sfc_MULTILINESTRING"))==1){
    plot(sf::st_geometry(x), col = colVec, lwd = lwd, add = add)
  }else{
    plot(sf::st_geometry(x), col = colVec, border = border, lwd = lwd, 
         add = add)
  }
  
  legendChoro(pos = legend.pos, 
              title.txt = legend.title.txt,
              title.cex = legend.title.cex,
              values.cex = legend.values.cex,
              breaks = layer$distr, 
              col = layer$col, 
              values.rnd = legend.values.rnd,
              frame = legend.frame, 
              symbol="box",  nodata.col = colNA,
              nodata = nodata, 
              nodata.txt = legend.nodata, 
              border = legend.border, 
              horiz = legend.horiz)
  
  
}
