#' @title Proportional Symbols Typo Layer
#' @name propSymbolsTypoLayer
#' @description Plot a proportional symbols layer with a color based on a
#' qualitative data.
#' @param spdf SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df a data frame that contains the values to plot.
#' @param spdfid identifier field in spdf, default to the first column
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column
#' of df. (optional)
#' @param var name of the numeric field in df to plot the symbols sizes.
#' @param var2 name of the factor (or character) field in df to plot.
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param col a vector of colors.
#' @param inches size of the biggest symbol (radius for circles, width for
#' squares, height for bars) in inches.
#' @param k share of the map occupied by the biggest symbol (this argument
#' is deprecated; please use inches instead.).
#' @param fixmax value of the biggest symbol. (optional)
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param legend.var.pos position of the legend for var, one of "topleft", "top",
#' @param legend.var2.pos position of the legend for var2, one of "topleft", "top",
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param legend.var.title.txt title of the legend (numeric data).
#' @param legend.var2.title.txt title of the legend (factor data).
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in
#' the legend.
#' @param legend.var.style either "c" or "e". The legend has two display
#' styles, "c" stands for compact and "e" for extended.
#' @param legend.var.frame whether to add a frame to the legend (TRUE) or
#' not (FALSE).
#' @param legend.var2.frame whether to add a frame to the legend (TRUE) or
#' not (FALSE).
#' @param legend.var2.nodata text for "no data" values
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE).
#' @export
#' @import sp
#' @seealso \link{legendBarsSymbols}, \link{legendTypo},
#' \link{legendCirclesSymbols}, \link{legendSquaresSymbols},
#' \link{typoLayer}, \link{propSymbolsLayer}
#' @examples
#' data("nuts2006")
#' ## Example 1
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' propSymbolsTypoLayer(spdf = nuts0.spdf, df = nuts0.df,
#'                      var = "pop2008", var2="typo")
#' 
#' ## Example 2
#' # Layout plot
#' layoutLayer(title = "Countries Population & Color in Europe",
#'             sources = "UMS RIATE, 2015",
#'             scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white",
#'             bg = "#D9F5FF",
#'             extent = nuts0.spdf)
#' #Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=TRUE)
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' nuts0.df$typo[1:3] <- NA
#' nuts0.df$pop2008[4:6] <- NA
#' propSymbolsTypoLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                      var = "pop2008", var2="typo", 
#'                      symbols = "circle",
#'                      legend.var.pos = "topright", 
#'                      legend.var2.pos = "right",
#'                      legend.var.title.txt = "Total\npopulation (2008)",
#'                      legend.values.rnd = -3,
#'                      legend.var2.title.txt = "Category",
#'                      legend.var.style = "c")
propSymbolsTypoLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                                 inches = 0.3, fixmax = NULL, symbols = "circle",
                                 border = "grey20", lwd = 1,
                                 var2, col = NULL,
                                 legend.title.cex = 0.8,
                                 legend.values.cex = 0.6,
                                 legend.var.pos = "bottomleft",
                                 legend.var.title.txt = var,
                                 legend.values.rnd = 0,
                                 legend.var.style = "c",
                                 legend.var.frame = FALSE,
                                 legend.var2.pos = "topright",
                                 legend.var2.title.txt = var2,
                                 legend.var2.nodata = "no data",
                                 legend.var2.frame = FALSE,
                                 add = TRUE, k = NULL){
  if(!is.null(k)){
    warning("Argument k is deprecated; please use inches instead.",
            call. = FALSE)
    propSymbolsTypoLayer2(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid,
                          var = var,
                          k = k, fixmax = fixmax, symbols = symbols,
                          border = border, lwd = lwd,
                          var2 = var2, col = col,
                          legend.title.cex = legend.title.cex,
                          legend.values.cex = legend.values.cex,
                          legend.var.pos = legend.var.pos,
                          legend.var.title.txt = legend.var.title.txt,
                          legend.values.rnd = legend.values.rnd,
                          legend.var.style = legend.var.style,
                          legend.var.frame = legend.var.frame,
                          legend.var2.pos = legend.var2.pos,
                          legend.var2.title.txt = legend.var2.title.txt,
                          legend.var2.nodata = legend.var2.nodata,
                          legend.var2.frame = legend.var2.frame,
                          add = add)
  }else{
    # check merge and order spdf & df
    dots <- checkMergeOrder(spdf = spdf, spdfid = spdfid,
                            df = df, dfid = dfid, var = var)
    
    
    # Color Management
    mycols <- as.factor(dots[, var2])
    if (!is.null(col)){
      levels(mycols) <- col
    } else {
      col <- grDevices::rainbow(nlevels(mycols))
      levels(mycols) <- col
    }

    # for the legend
    mycolsleg <- as.character(levels(mycols))
    rVal <- as.character(levels(as.factor(dots[, var2])))
    mycols <- as.character(mycols)

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
             symbols(dots[, 2:3], circles = sizes, bg = mycols, fg = border, 
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
                                    values.rnd =  legend.values.rnd,
                                    style = legend.var.style)
             }
           }, 
           square = {
             symbols(dots[, 2:3], squares = sizes, bg = mycols, fg = border, 
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
                                    values.rnd =  legend.values.rnd,
                                    style = legend.var.style)
             }
           }, 
           bar = {
             tmp <- as.matrix(data.frame(width = inches/10, height = sizes))
             dots[,3] <- dots[,3] + yinch(sizes/2)
             symbols(dots[,2:3], rectangles = tmp, add = TRUE, bg = mycols,
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
                                 values.rnd =  legend.values.rnd,
                                 style = legend.var.style)
             }
           })
    nodata <- FALSE
    if(max(is.na(dots[,var2])>0)){nodata <- TRUE}
    
    if(legend.var2.pos !="n"){
      legendTypo(pos = legend.var2.pos,
                 title.txt = legend.var2.title.txt,
                 title.cex = legend.title.cex,
                 values.cex = legend.values.cex,
                 categ = rVal,
                 col = mycolsleg,
                 frame = legend.var2.frame,
                 symbol="box",
                 nodata = nodata,nodata.col = NA,
                 nodata.txt = legend.var2.nodata)
    }
  }
}

