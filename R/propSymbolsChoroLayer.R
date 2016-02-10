#' @title Proportional and Choropleth Symbols Layer
#' @name propSymbolsChoroLayer
#' @description Plot a proportional symbols layer with color based on a 
#' quantitative data discretization. 
#' @param spdf SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df a data frame that contains the values to plot.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param inches size of the biggest symbol (radius for circles, width for
#' squares, height for bars) in inches.
#' @param var name of the numeric field in df to plot the symbols sizes.
#' @param var2 name of the numeric field in df to plot the colors.
#' @param breaks break points in sorted order to indicate the intervals for assigning the colors. 
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) 
#' breakpoints (see \link{choroLayer} Details).
#' @param col a vector of colors. Note that if breaks is specified there must be one less 
#' colors specified than the number of break. 
#' @param nclass a targeted number of classes. If null, the number of class is 
#' automatically defined (see \link{choroLayer} Details).
#' @param method a discretization method; one of "sd", "equal", 
#' "quantile", "jenks", "q6" or "geom"  (see \link{choroLayer} Details).
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param k share of the map occupied by the biggest symbol (this argument
#' is deprecated; please use inches instead.).
#' @param fixmax value of the biggest symbol (see \link{propSymbolsLayer} Details).
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param legend.var.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.var.pos is "n" then the legend is not plotted.
#' @param legend.var2.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.var2.pos is "n" then the legend is not plotted.
#' @param legend.var.title.txt title of the legend (proportional symbols).
#' @param legend.var2.title.txt title of the legend (colors).
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.var.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.var2.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.var.style either "c" or "e". The legend has two display 
#' styles.
#' @param legend.var.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.nodata text for "no data" values
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @examples
#' data("nuts2006")
#' ## Example 1
#' # Growth rate
#' nuts0.df$cagr <- (((nuts0.df$pop2008 / nuts0.df$pop1999)^(1/9)) - 1) * 100
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Plot the symbols
#' propSymbolsChoroLayer(spdf = nuts0.spdf, df = nuts0.df,symbols = "circle", 
#'                       var = "pop2008", var2 = "cagr")
#' 
#' ## Example 2
#' # Growth rate at nuts2 level
#' nuts2.df$cagr <- (((nuts2.df$pop2008 / nuts2.df$pop1999)^(1/9)) - 1) * 100
#' 
#' # First layout
#' layoutLayer(title="Demographic trends, 1999-2008",
#'             scale = NULL,col = NA, coltitle = "black",
#'             sources = "", author = "",
#'             frame = FALSE, bg = "#A6CAE0",
#'             south = TRUE, extent = nuts0.spdf)
#' plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
#' plot(nuts2.spdf, col = "grey60",border = "white", lwd=0.4, add=TRUE)
#' 
#' # Add some NA values
#' nuts2.df[1:10,"pop2008"] <- NA
#' nuts2.df[100:110,"cagr"] <- NA
#' 
#' # Plot symbols
#' propSymbolsChoroLayer(spdf = nuts2.spdf, df = nuts2.df, 
#'                       var = "pop2008", var2 = "cagr", 
#'                       inches = 0.1,
#'                       col = carto.pal(pal1 = "blue.pal", n1 = 2, 
#'                                       pal2 = "red.pal", n2 = 4), 
#'                       breaks = c(-2.43,-1,0,0.5,1,2,3.1),
#'                       border = "grey50", lwd = 1,
#'                       legend.var.pos = "topright", legend.var2.pos = "right",
#'                       legend.var2.title.txt = "Compound annual\ngrowth rate",
#'                       legend.var.title.txt = "Total Population",
#'                       legend.var.style = "e")
#' # Second layout
#' layoutLayer(title = "", author = "Eurostat, 2011", 
#'             sources = "", frame ="", col = NA)
#' @export
#' @seealso \link{legendBarsSymbols}, \link{legendChoro}, 
#' \link{legendCirclesSymbols}, \link{legendSquaresSymbols}, 
#' \link{choroLayer}, \link{propSymbolsLayer}
#' @import sp
propSymbolsChoroLayer <- function(spdf, df, spdfid = NULL, dfid = NULL,
                                  var, 
                                  inches = 0.3, fixmax = NULL, 
                                  symbols = "circle", border = "grey20", lwd = 1,
                                  var2, 
                                  breaks=NULL,  method="quantile",  nclass=NULL, 
                                  col = NULL,
                                  legend.title.cex = 0.8, 
                                  legend.values.cex = 0.6,
                                  legend.var.pos = "right",
                                  legend.var.title.txt = var, 
                                  legend.var.values.rnd = 0, 
                                  legend.var.style = "c",
                                  legend.var.frame = FALSE, 
                                  legend.var2.pos = "topright", 
                                  legend.var2.title.txt = var2,
                                  legend.var2.values.rnd = 2,  
                                  legend.var2.nodata = "no data",
                                  legend.var2.frame = FALSE,
                                  add = TRUE, k = NULL){
  
  if(!is.null(k)){
    warning("Argument k is deprecated; please use inches instead.",
            call. = FALSE)
    propSymbolsChoroLayer2(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid,
                          var = var,
                          k = k, fixmax = fixmax, symbols = symbols,
                          breaks = breaks, method = method, nclass = class,
                          border = border, lwd = lwd,
                          var2 = var2, col = col,
                          legend.title.cex = legend.title.cex,
                          legend.values.cex = legend.values.cex,
                          legend.var.pos = legend.var.pos,
                          legend.var.title.txt = legend.var.title.txt,
                          legend.var.values.rnd = legend.var.values.rnd,
                          legend.var.style = legend.var.style,
                          legend.var.frame = legend.var.frame,
                          legend.var2.pos = legend.var2.pos,
                          legend.var2.title.txt = legend.var2.title.txt,
                          legend.var2.values.rnd = legend.var2.values.rnd,
                          legend.var2.nodata = legend.var2.nodata,
                          legend.var2.frame = legend.var2.frame,
                          add = add)
  }else{
    # check merge and order spdf & df
    dots <- checkMergeOrder(spdf = spdf, spdfid = spdfid,
                            df = df, dfid = dfid, var = var)
    
    
    # Color Management
    layer <- choro(var=dots[,var2], distr = breaks, col = col,
                   nclass = nclass, method = method)
    
    mycols <- layer$colMap

    if (is.null(fixmax)){
      fixmax <- max(dots[,var])
    }
    
    # size management
    sizes <- sizer(dots = dots, inches = inches, var = var,
                   fixmax = fixmax, symbols = symbols)
    sizeMax <- max(sizes)
    
    if (inches <= sizeMax){
      sizevect <- xinch(seq(inches, inches/9, length.out = 4))
      varvect <- seq(fixmax,0,length.out = 4 )
      inches <- sizeMax
    }else{
      mycols <- c(NA, mycols)
      border <- c(NA, rep(border, nrow(dots)))
      dots <- rbind(dots[1,],dots)
      dots[1,var] <- fixmax
      sizes <- c(inches, sizes)
      sizevect <- xinch(seq(inches, inches/9, length.out = 4))
      varvect <- seq(fixmax, 0,length.out = 4 )
    }
    
    if (add==FALSE){
      sp::plot(spdf, col = NA, border = NA)
    }
    switch(symbols, 
           circle = {
             symbols(dots[, 2:3], circles = sizes, bg = as.vector(mycols), 
                     fg = border, 
                     lwd = lwd, add = TRUE, inches = inches, asp = 1)
             if(legend.var.pos!="n"){
               legendCirclesSymbols(pos = legend.var.pos, 
                                    title.txt = legend.var.title.txt,
                                    title.cex = legend.title.cex,
                                    values.cex = legend.values.cex,
                                    var = varvect,
                                    r = sizevect,
                                    col = "grey",
                                    frame = legend.var.frame,
                                    values.rnd =  legend.var.values.rnd,
                                    style = legend.var.style)
             }
           }, 
           square = {
             symbols(dots[, 2:3], squares = sizes, bg = as.vector(mycols), 
                     fg = border, 
                     lwd = lwd, add = TRUE, inches = inches, asp = 1)
             if(legend.var.pos!="n"){
               legendSquaresSymbols(pos = legend.var.pos, 
                                    title.txt = legend.var.title.txt,
                                    title.cex = legend.title.cex,
                                    values.cex = legend.values.cex,
                                    var = varvect,
                                    r = sizevect,
                                    col = "grey",
                                    frame = legend.var.frame,
                                    values.rnd =  legend.var.values.rnd,
                                    style = legend.var.style)
             }
           }, 
           bar = {
             tmp <- as.matrix(data.frame(width = inches/10, height = sizes))
             dots[,3] <- dots[,3] + yinch(sizes/2)
             symbols(dots[,2:3], rectangles = tmp, add = TRUE, 
                     bg = as.vector(mycols),
                     fg = border, lwd = lwd, inches = inches, asp = 1)
             if(legend.var.pos!="n"){
               legendBarsSymbols(pos = legend.var.pos, 
                                 title.txt = legend.var.title.txt,
                                 title.cex = legend.title.cex,
                                 values.cex = legend.values.cex,
                                 var = varvect,
                                 r = sizevect,
                                 col = "grey",
                                 frame = legend.var.frame,
                                 values.rnd =  legend.var.values.rnd,
                                 style = legend.var.style)
             }
           })
    nodata <- FALSE
    if(max(is.na(dots[,var2])>0)){nodata <- TRUE}
    
    if(legend.var2.pos !="n"){
      legendChoro(pos = legend.var2.pos, 
                  title.txt = legend.var2.title.txt,
                  title.cex = legend.title.cex,
                  values.cex = legend.values.cex,
                  breaks = layer$distr, 
                  col = layer$col, 
                  values.rnd = legend.var2.values.rnd,
                  frame = legend.var2.frame, 
                  symbol="box", 
                  nodata = nodata, nodata.col = NA,
                  nodata.txt = legend.var2.nodata)
    }
  }
}

