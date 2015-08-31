#' @title Proportional Symbols Typo Layer
#' @name propSymbolsTypoLayer
#' @description Plot a proportional symbols layer with a color based on a qualitative data. Various symbols are availables.
#' @param spdf a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df a data.frame that contains the values to plot.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data.frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df to plot.
#' @param var2 name of the factor field in df to plot.
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param col a vector of colors.
#' @param k share of the map occupied by the biggest symbol.
#' @param fixmax value of the biggest symbol. (optional)
#' @param legend.var.pos position of the legend for var, one of "topleft", "top", 
#' @param legend.var2.pos position of the legend for var2, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param legend.title.var.txt title of the legend (numeric data).
#' @param legend.title.var2.txt title of the legend (factor data).
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.style either "a" or "b". The legend has two display 
#' styles.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @export
#' @import sp
#' @examples
#' data("nuts2006")
#' # Layout plot
#'layoutLayer(title = "Countries Population in Europe", 
#'            sources = "UMS RIATE, 2015", 
#'            author = "UMS RIATE",
#'            scale = NULL, 
#'            frame = TRUE,
#'            col = "black", 
#'           coltitle = "white",
#'            bg = "#D9F5FF",
#'            south = TRUE, 
#'            extent = nuts0.spdf)
#' #Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=TRUE)
#' #Population plot on proportional symbols
#'#colours <- palette(rainbow(length(nuts0.spdf)))
#'#propSymbolsTypoLayer(spdf = nuts0.spdf, df = nuts0.df,
#'#                     var = "pop2008", var2="id", k = 0.01,
#'#                     symbols = "circles", col =  colours,
#'#                     legend.var.pos = "topleft", legend.var2.pos = "topright",
#'#                     legend.title.var.txt = "Total\npopulation (2008)",
#'#                     legend.title.var2.txt = "Types de trucs",
#'#                     legend.style = "b")
propSymbolsTypoLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,var2,
                             symbols = "circles",
                             col = NULL,
                             k = 0.02, fixmax = NULL,
                             legend.var.pos = "bottomleft",legend.var2.pos = "bottomleft", legend.title.var.txt = var,
                             legend.title.var2.txt = var2,
                             legend.title.cex = 0.8, legend.values.cex = 0.6,
                             legend.style = "a", legend.frame = FALSE,
                             legend.values.rnd = 0, add = TRUE){
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  dots <- cbind(spdf@data[,spdfid], as.data.frame(sp::coordinates(spdf)))
  colnames(dots) <- c(spdfid, "x", "y")
  dots <- data.frame(dots, df[match(dots[,spdfid], df[,dfid]),])
  dots <- dots[order(dots[, var], decreasing = TRUE),]
  
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
  
  #dots$var2
  colours <- col
  dots$col <- as.factor(dots[, var2])
  levels(dots$col) <- colours
  mycols <- as.character(dots$col)
  rVal <- as.character(levels(dots[, var2]))
  

  # CIRCLES
  if (symbols == "circles"){
    symbols(dots[, c("x", "y")], circles = dots$circleSize, bg = mycols,
            add = add,
            inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$circleSize
    varvect <- dots[,var]
#     LegendCircSymbols(pos = legend.var.pos, legTitle = legend.title.var.txt,
#                       legTitleCex = legend.title.cex,
#                       legValuesCex = legend.values.cex,
#                       varvect = varvect,
#                       sizevect = sizevect,
#                       breakval  = NULL,
#                       col1 = "white",
#                       col2 = "white",
#                       frame = legend.frame,
#                       round = legend.values.rnd,
#                       type = legend.style)
     }
  
  # SQUARES
  if (symbols == "squares"){
    symbols(dots[, c("x", "y")], squares = dots$squareSize, bg = mycols,
            add = add, inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$squareSize
    varvect <- dots[,var]

  }
  
  #BARRES
  if (symbols == "height"){
    width<-min((par()$usr[4]-par()$usr[3])/40,(par()$usr[2]-par()$usr[1])/40)
    tmp <- as.matrix(data.frame(width,dots$heightSize))
    dots$y2 <- dots$y+dots$heightSize/2
    symbols(dots[,c("x","y2")], rectangles = tmp, add = add, bg = mycols,
            inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$heightSize
    varvect <- dots[,var]

    
  }
#   # CAISSONS TYPO
#   LegendTypo(pos = legend.var2.pos, legTitle = legend.title.var2.txt,
#              legTitleCex = legend.title.cex, legValuesCex = legend.values.cex,
#              categ = rVal,
#              cols = colours,
#              nodatalabel = "Absence\nd'information",nodata = FALSE,
#              frame=legend.frame,symbol="box")
  
}

