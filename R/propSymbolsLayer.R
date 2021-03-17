#' @title Proportional Symbols Layer
#' @name propSymbolsLayer
#' @description Plot a proportional symbols layer.
#' @param x an sf object, a simple feature collection. If x is used then spdf, df, spdfid and dfid are not.
#' @param spdf a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df a data frame that contains the values to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df to plot.
#' @param inches size of the biggest symbol (radius for circles, width for
#' squares, height for bars) in inches.
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param col color of symbols.
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param fixmax value of the biggest symbol (see Details).
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values 
#' displayed in the legend.
#' @param legend.style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details  
#' Two maps with the same inches and fixmax parameters will be comparable.
#' @export 
#' @seealso \link{legendBarsSymbols}, \link{legendCirclesSymbols}, 
#' \link{legendSquaresSymbols}, \link{propSymbolsChoroLayer}, 
#' \link{propSymbolsTypoLayer}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' propSymbolsLayer(x = mtq, var = "POP")
#' 
#' plot(st_geometry(mtq), col = "lightblue4",border = "lightblue3", 
#'      bg = "lightblue1")
#' # Population plot on proportional symbols
#' propSymbolsLayer(x = mtq, var = "POP",
#'                  symbols = "circle", col =  "white",
#'                  legend.pos = "right", border = "grey",
#'                  legend.title.txt = "Total\nPopulation",
#'                  legend.style = "c")
#' # Layout plot
#' layoutLayer(title = "Population Distribution in Martinique, 2015")
propSymbolsLayer <- function(x, spdf, df, spdfid = NULL, dfid = NULL, var,
                             inches = 0.3, fixmax = NULL, 
                             symbols = "circle", 
                             col = "#E84923", 
                             border = "black", lwd = 1,
                             legend.pos = "bottomleft", 
                             legend.title.txt = var,
                             legend.title.cex = 0.8, 
                             legend.values.cex = 0.6,
                             legend.values.rnd = 0,
                             legend.style = "c", 
                             legend.frame = FALSE,
                             add = TRUE){

  if (missing(x)){
    x <- convertToSf(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid)
  }
  
  # check merge and order spdf & df
  dots <- checkMergeOrder(x = x, var = var)
  
  
  # Double color management
  mycols <- rep(col, nrow(dots))
  
  if (is.null(fixmax)){
    fixmax <- max(dots[[var]])
  }
  
  # compute sizes
  sizes <- sizer(dots = dots, inches = inches, var = var, 
                 fixmax = fixmax, symbols = symbols)
  
  
  # size and values for legend, hollow circle (fixmax case)
  sizeMax <- max(sizes)
  if (inches <= sizeMax){
    sizevect <- xinch(seq(inches, min(sizes), length.out = 4))
    varvect <- seq(fixmax, 0, length.out = 4)
    inches <- sizeMax
  }else{
    mycols <- c(NA, mycols)
    border <- c(NA, rep(border, nrow(dots)))
    dots <- rbind(dots[1,],dots)
    dots[1,var] <- fixmax
    sizes <- c(inches, sizes)
    sizevect <- xinch(seq(inches, min(sizes), length.out = 4))
    varvect <- seq(fixmax, 0,length.out = 4 )
  }
  
  # plot
  if (add==FALSE){
    bbx <- sf::st_bbox(x)
    plot(0, type='n', axes = FALSE, ann = FALSE, asp = 1, 
         xlim = bbx[c(1,3)], ylim = bbx[c(2,4)])
  }
  
  switch(symbols, 
         circle = {
           symbols(dots[, 1:2, drop = TRUE], circles = sizes, bg = mycols, 
                   fg = border, lwd = lwd, add = TRUE, inches = inches, asp = 1)
           legendCirclesSymbols(pos = legend.pos, 
                                title.txt = legend.title.txt,
                                title.cex = legend.title.cex,
                                values.cex = legend.values.cex,
                                var = c(min(dots[[var]]),max(dots[[var]])),
                                inches = inches,
                                col = col, lwd = lwd,
                                frame = legend.frame,
                                values.rnd =  legend.values.rnd,
                                style = legend.style)
         }, 
         square = {
           symbols(dots[, 1:2, drop = TRUE], squares = sizes, bg = mycols, 
                   fg = border, lwd = lwd, add = TRUE, inches = inches, asp = 1)
           legendSquaresSymbols(pos = legend.pos,
                                title.txt = legend.title.txt,
                                title.cex = legend.title.cex,
                                values.cex = legend.values.cex,
                                var = c(min(dots[[var]]),max(dots[[var]])),
                                inches = inches,
                                col = col, lwd = lwd, 
                                frame = legend.frame,
                                values.rnd =  legend.values.rnd,
                                style = legend.style)
         }, 
         bar = {
           tmp <- as.matrix(data.frame(width = inches/7, height = sizes))
           dots[[2]] <- dots[[2]] + yinch(sizes/2)
           symbols(dots[, 1:2, drop = TRUE], rectangles = tmp, add = TRUE, 
                   bg = mycols,fg = border, lwd = lwd, inches = inches, asp = 1)
           legendBarsSymbols(pos = legend.pos, 
                             title.txt = legend.title.txt,
                             title.cex = legend.title.cex,
                             values.cex = legend.values.cex,
                             var = c(min(dots[[var]]),max(dots[[var]])),
                             inches = inches,
                             col = col, lwd = lwd,
                             frame = legend.frame,
                             values.rnd =  legend.values.rnd,
                             style = legend.style)
         })
  
}
