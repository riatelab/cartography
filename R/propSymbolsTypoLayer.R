#' @title Proportional Symbols Typo Layer
#' @name propSymbolsTypoLayer
#' @description Plot a proportional symbols layer with a color based on a 
#' qualitative data. Various symbols are availables.
#' @param spdf SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df a data.frame that contains the values to plot.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data.frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df to plot the symbols sizes.
#' @param var2 name of the factor (or character) field in df to plot.
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param col a vector of colors.
#' @param k share of the map occupied by the biggest symbol.
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
#' @examples
#' data("nuts2006")
#' # Layout plot
#' layoutLayer(title = "Countries Population in Europe",
#'             sources = "UMS RIATE, 2015",
#'             author = "UMS RIATE",
#'             scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white",
#'             bg = "#D9F5FF",
#'             extent = nuts0.spdf)
#' #Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=TRUE)
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' propSymbolsTypoLayer(spdf = nuts0.spdf, df = nuts0.df,
#'                      var = "pop2008", var2="typo", k = 0.01,
#'                      symbols = "circle", 
#'                      legend.var.pos = "topright", legend.var2.pos = "right",
#'                      legend.var.title.txt = "Total\npopulation (2008)",
#'                      legend.values.rnd = -3,
#'                      legend.var2.title.txt = "Category",
#'                      legend.var.style = "c")
propSymbolsTypoLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,var2,
                                 symbols = "circles",
                                 col = NULL,
                                 k = 0.02, fixmax = NULL,
                                 border = "grey20", lwd = 1,
                                 legend.var.pos = "bottomleft",
                                 legend.var2.pos = "bottomleft", 
                                 legend.var.title.txt = var,
                                 legend.var2.title.txt = var2,
                                 legend.title.cex = 0.8, 
                                 legend.values.cex = 0.6,
                                 legend.var.style = "c", 
                                 legend.var.frame = FALSE,
                                 legend.var2.frame = FALSE,
                                 legend.values.rnd = 0,
                                 legend.var2.nodata = "no data",
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
  
  dots$col <- as.factor(dots[, var2])

  if (!is.null(col)){
    levels(dots$col) <- col
  } else {
    col <- grDevices::rainbow(nlevels(dots$col))
    levels(dots$col) <- col
  }
  
  
  # for the legend  
  mycols <- as.character(levels(dots$col))
  rVal <- as.character(levels(as.factor(dots[, var2])))

  
  # CIRCLES
  if (symbols == "circle"){
    symbols(dots[, 2:3], circles = dots$circleSize, bg = as.vector(dots$col),
            add = TRUE,
            fg = border, lwd = lwd,
            inches = FALSE, asp = 1, xlab = "", ylab = "")
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
                           values.rnd =  legend.values.rnd,
                           style = legend.var.style)
    }
    
    
    
    
  }
  
  
  
  
  # SQUARES
  if (symbols == "square"){
    symbols(dots[, 2:3], squares = dots$squareSize,  bg = as.vector(dots$col),
            add = TRUE, inches = FALSE, 
            fg = border, lwd = lwd,
            asp = 1, xlab = "", ylab = "")
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
                           values.rnd =  legend.values.rnd,
                           style = legend.var.style)
    }
    
  }
  
  #BARRES
  if (symbols == "bar"){
    width<-min((par()$usr[4] - par()$usr[3]) / 40, 
               (par()$usr[2] - par()$usr[1]) / 40)
    tmp <- as.matrix(data.frame(width, dots$heightSize))
    dots[,3] <- dots[,3] + dots$heightSize / 2
    symbols(dots[,2:3], rectangles = tmp, add = TRUE, bg = as.vector(dots$col),
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
                        values.rnd =  legend.values.rnd,
                        style = legend.var.style)
      
    }
    
    
    
    
  }
  nodata <- FALSE
  if(max(is.na(df[,var])>0)){nodata <- TRUE}

  if(legend.var2.pos !="n"){
    legendTypo(pos = legend.var2.pos, 
               title.txt = legend.var2.title.txt,
               title.cex = legend.title.cex, 
               values.cex = legend.values.cex,
               categ = rVal, 
               col = mycols, 
               frame = legend.var2.frame, 
               symbol="box", 
               nodata = nodata, 
               nodata.txt = legend.var2.nodata)
    
  }
}

