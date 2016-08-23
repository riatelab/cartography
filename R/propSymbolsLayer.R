#' @title Proportional Symbols Layer
#' @name propSymbolsLayer
#' @description Plot a proportional symbols layer.
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
#' @param k share of the map occupied by the biggest symbol (this argument
#' is deprecated; please use inches instead.).
#' @param col color of symbols.
#' @param col2 second color of symbols (see Details).
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param breakval breaking value (see Details).
#' @param fixmax value of the biggest symbol (see Details).
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
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
#' @details The breakval parameter allows to plot symbols of two 
#' colors: the first color (col) for values superior or equal to breakval,
#' second color (col2) for values inferior to breakval.
#' 
#' Two maps with the same inches and fixmax parameters will be comparable.
#' @export
#' @seealso \link{legendBarsSymbols}, \link{legendCirclesSymbols}, 
#' \link{legendSquaresSymbols}, \link{propSymbolsChoroLayer}, 
#' \link{propSymbolsTypoLayer}
#' @import sp
#' @examples
#' data("nuts2006")
#' ## Example 1
#' # Layout plot
#' layoutLayer(title = "Countries Population in Europe",
#'             sources = "Eurostat, 2008",
#'             scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white",
#'             bg = "#D9F5FF",
#'             south = TRUE,
#'             extent = nuts0.spdf)
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=TRUE)
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df,
#'                  var = "pop2008", 
#'                  symbols = "square", col =  "#920000",
#'                  legend.pos = "right",
#'                  legend.title.txt = "Total\npopulation (2008)",
#'                  legend.style = "c")
#' 
#' ## Example 2
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df,
#'                  var = "gdppps2008",
#'                  symbols = "bar", col =  "#B00EF0",
#'                  legend.pos = "right",
#'                  legend.title.txt = "GDP\nin Millions PPS (2008)",
#'                  legend.style = "e")
#' 
#' ## Example 3
#' oldpar <- par(mfrow = c(1,2), mar = c(0,0,0,0))
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df,
#'                  var = "birth_2008", 
#'                  fixmax = max(nuts0.df$birth_2008),
#'                  inches = 0.2,
#'                  symbols = "circle", col =  "orange",
#'                  legend.pos = "right",
#'                  legend.title.txt = "nb of births",
#'                  legend.style = "e")
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df,
#'                  var = "death_2008",
#'                  symbols = "circle", col =  "pink",
#'                  fixmax = max(nuts0.df$birth_2008),
#'                  inches = 0.2,
#'                  legend.pos = "right",
#'                  legend.style = "e",
#'                  legend.title.txt = "nb of deaths")
#' par(oldpar)
#' 
#' ## Example 4
#' nuts0.df$balance <- nuts0.df$birth_2008-nuts0.df$death_2008
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, inches = 0.3,
#'                  var = "balance",
#'                  symbols = "circle",
#'                  col = "orange", col2 = "green", breakval=0,
#'                  legend.pos = "right",
#'                  legend.style = "c",
#'                  legend.title.txt = "Natural Balance\n(2008)")
propSymbolsLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                             inches = 0.3, fixmax = NULL, 
                             breakval = NULL,
                             symbols = "circle", 
                             col = "#E84923", col2 = "#7DC437", 
                             border = "black", lwd = 1,
                             legend.pos = "bottomleft", 
                             legend.title.txt = var,
                             legend.title.cex = 0.8, 
                             legend.values.cex = 0.6,
                             legend.values.rnd = 0,
                             legend.style = "c", 
                             legend.frame = FALSE,
                             add = TRUE, k = NULL){
  # info about k
  if(!is.null(k)){
    stop("Argument k is deprecated (last used in version 1.3.0); please use inches instead.",
         call. = FALSE)
  }

  # Check missing df and NULL identifiers 
  if (missing(df)){df <- spdf@data}
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  
  # check merge and order spdf & df
  dots <- checkMergeOrder(spdf = spdf, spdfid = spdfid, 
                          df = df, dfid = dfid, var = var)
  

  # Double color management
  if (!is.null(breakval)){
    mycols <- rep(NA,nrow(dots))
    mycols <- ifelse(test = dots[,var] >= breakval, 
                     yes = col,
                     no = col2)
  }else{
    mycols <- rep(col, nrow(dots))
  }
  
  if (is.null(fixmax)){
    fixmax <- max(dots[,var])
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
    sp::plot(spdf, col = NA, border = NA)
  }
  
  switch(symbols, 
         circle = {
           symbols(dots[, 2:3], circles = sizes, bg = mycols, fg = border, 
                   lwd = lwd, add = TRUE, inches = inches, asp = 1)
           if(legend.pos!="n"){
             legendCirclesSymbols(pos = legend.pos, 
                                  title.txt = legend.title.txt,
                                  title.cex = legend.title.cex,
                                  values.cex = legend.values.cex,
                                  var = varvect,
                                  r = sizevect,
                                  breakval  = breakval,
                                  col = col,
                                  col2 = col2,
                                  frame = legend.frame,
                                  values.rnd =  legend.values.rnd,
                                  style = legend.style)
           }
         }, 
         square = {
           symbols(dots[, 2:3], squares = sizes, bg = mycols, fg = border, 
                   lwd = lwd, add = TRUE, inches = inches, asp = 1)
           if(legend.pos!="n"){
             legendSquaresSymbols(pos = legend.pos,
                                  title.txt = legend.title.txt,
                                  title.cex = legend.title.cex,
                                  values.cex = legend.values.cex,
                                  var = varvect,
                                  r = sizevect,
                                  breakval  = breakval,
                                  col = col,
                                  col2 = col2,
                                  frame = legend.frame,
                                  values.rnd =  legend.values.rnd,
                                  style = legend.style)
           }
         }, 
         bar = {
           tmp <- as.matrix(data.frame(width = inches/10, height = sizes))
           dots[,3] <- dots[,3] + yinch(sizes/2)
           symbols(dots[,2:3], rectangles = tmp, add = TRUE, bg = mycols,
                   fg = border, lwd = lwd, inches = inches, asp = 1)
           if(legend.pos!="n"){
             legendBarsSymbols(pos = legend.pos, 
                               title.txt = legend.title.txt,
                               title.cex = legend.title.cex,
                               values.cex = legend.values.cex,
                               var = varvect,
                               r = sizevect,
                               breakval  = breakval,
                               col = col,
                               col2 = col2,
                               frame = legend.frame,
                               values.rnd =  legend.values.rnd,
                               style = legend.style)
           }
         })
  
}
