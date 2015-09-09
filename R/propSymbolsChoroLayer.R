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
#' @param k share of the map occupied by the biggest symbol (see \link{propSymbolsLayer} Details).
#' @param fixmax value of the biggest symbol (see \link{propSymbolsLayer} Details).
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param legend.var.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.var.pos is "n" then the legend is not plotted.
#' @param legend.var2.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.var2.pos is "n" then the legend is not plotted.
#' @param legend.var.title.txt title of the legend (proportionnal symbols).
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
#' ## Exemple 1
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population and growth rate 
#' nuts0.df$cagr <- (((nuts0.df$pop2008 / nuts0.df$pop1999)^(1/9)) - 1) * 100
#' propSymbolsChoroLayer(spdf = nuts0.spdf, df = nuts0.df,var = "pop2008", var2 = "cagr", 
#'                       legend.var.values.rnd = -3, legend.var2.values.rnd = 2)
#' @export
#' @import sp
propSymbolsChoroLayer <- function(spdf, df, spdfid = NULL, dfid = NULL,
                                  var, 
                                  k = 0.02, fixmax = NULL, 
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
                                  legend.var2.values.rnd = 0,  
                                  legend.var2.nodata = "no data",
                                  legend.var2.frame = FALSE,
                                  add = TRUE){
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  dots <- cbind(spdf@data[,spdfid], as.data.frame(sp::coordinates(spdf)))
  colnames(dots)[1] <- c(spdfid)
  dots <- data.frame(dots, df[match(dots[,spdfid], df[,dfid]),])
  dots <- dots[order(abs(dots[, var]), decreasing = TRUE),]
  
  x1 <- sp::bbox(spdf)[1]
  y1 <- sp::bbox(spdf)[2]
  x2 <- sp::bbox(spdf)[3]
  y2 <- sp::bbox(spdf)[4]
  hfdc <- (x2-x1)
  sfdc <- (x2-x1)*(y2-y1)
  #   sc <- sum(abs(dots[,var]),na.rm = TRUE)
  sc <- max(abs(dots[,var]),na.rm = TRUE)
  if (is.null(fixmax)){
    dots$circleSize <- sqrt((abs(dots[, var]) * k * sfdc / sc) / pi)
    dots$squareSize <-  sqrt(abs(dots[, var]) * k * sfdc / sc)
    dots$heightSize <- abs(dots[,var]) * k * hfdc / sc * 10
  }
  
  if (!is.null(fixmax)){
    dots$circleSize <- sqrt((abs(dots[, var]) * k * sfdc / fixmax) / pi)
    dots$squareSize <-  sqrt(abs(dots[, var]) * k * sfdc / fixmax)
    dots$heightSize <- abs(dots[, var]) * k * hfdc / fixmax * 10
  }
  
  if (add==FALSE){
    sp::plot(spdf, col = NA, border = NA)
  }
  
  # get the colors and distr
  layer <- choro(var=dots[,var2], distr = breaks, col = col,
                 nclass = nclass, method = method)
  
  # CIRCLES
  if (symbols == "circle"){
    symbols(dots[, 2:3], circles = dots$circleSize, bg = as.vector(layer$colMap), 
            fg = border, lwd = lwd,
            add = TRUE,
            inches = FALSE, asp = 1	)
    sizevect <- dots$circleSize
    varvect <- dots[,var]
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
  }
  # SQUARES
  if (symbols == "square"){
    symbols(dots[, 2:3], squares = dots$squareSize, bg = as.vector(layer$colMap),
            fg = border, lwd = lwd,
            add = TRUE, inches = FALSE, asp = 1)
    sizevect <- dots$squareSize
    varvect <- dots[,var]
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
  }
  
  #BARRES
  if (symbols == "bar"){
    width<-min((par()$usr[4] - par()$usr[3]) / 40, (par()$usr[2] - par()$usr[1]) / 40)
    tmp <- as.matrix(data.frame(width, dots$heightSize))
    dots[,3] <- dots[,3] + dots$heightSize / 2
    symbols(dots[,2:3], rectangles = tmp, add = TRUE, bg = as.vector(layer$colMap),
            fg = border, lwd = lwd,
            inches = FALSE, asp = 1)
    sizevect <- dots$heightSize
    varvect <- dots[,var]
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
  }
  
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
                nodata = nodata, 
                nodata.txt = legend.var2.nodata)
    
  }
  
}

