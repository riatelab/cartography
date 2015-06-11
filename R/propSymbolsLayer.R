#' @title Proportional Symbols Layer
#' @name propSymbolsLayer
#' @description Plot a proportional symbols layer. Various symbols are availables.
#' @param spdf Spatial*DataFrame; if \code{spdf} is a SpatialPolygonsDataFrame 
#' symbols are plotted on centroids.
#' @param df data.frame; \code{df} contains the values to plot.
#' @param spdfid character; id field in \code{spdf}, default to the first column 
#' of the \code{spdf} data.frame. (optional)
#' @param dfid character; id field in \code{df}, default to the first column 
#' of \code{df}. (optional)
#' @param var character; name of the numeric field in \code{df} to plot.
#' @param symbols character; type of symbols, one of "circles", "squares" or "height").
#' @param col character; color of symbols.
#' @param col2 character; second color of symbols (see Details).
#' @param breakval numeric; breaking value (see Details).
#' @param k numeric; share of the map occupied by the biggest symbol.
#' @param fixmax numeric; value of the biggest symbol. (optional)
#' @param legend.pos character; position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param legend.title.txt character; title of the legend.
#' @param legend.title.cex numeric; size of the legend title.
#' @param legend.values.cex numeric; size of the values in the legend.
#' @param legend.values.rnd numeric; number of decimal places of the values in 
#' the legend.
#' @param legend.style character; either "a" or "b". The legend has two display 
#' styles.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details The (\code{breakval}) parameter allows to plot symbols of two 
#' colors: the first color (\code{col}) for values superior or equal to breakval,
#' second color (\code{col2}) for values inferior to breakval.
#' @return A plot is returned.
#' @export
#' @import sp
#' @examples
#' data("nuts2006")
#' 
#' # Layout plot
#' layoutLayer(title = "Countries Population in Europe", 
#'             sources = "UMS RIATE, 2015", 
#'             author = "UMS RIATE",
#'             scale = 0, 
#'             frame = TRUE,
#'             col = "black", 
#'             coltitle = "white",
#'             bg = "#D9F5FF",
#'             south = TRUE, 
#'             extent = nuts0.spdf)
#' 
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=TRUE)
#' 
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                  var = "pop2008", k = 0.01,
#'                  symbols = "squares", col =  "#920000",
#'                  legend.pos = "right", 
#'                  legend.title.txt = "Total\npopulation (2008)", 
#'                  legend.style = "a")

propSymbolsLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                             symbols = "circles",
                             col = "#E84923", col2 = "#7DC437", breakval = NULL,
                             k = 0.02, fixmax = NULL,
                             legend.pos = "bottomleft", legend.title.txt = var,
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
  
  if (!is.null(breakval)){
    dots$var2 <- ifelse(dots[, var] >= breakval,"sup","inf")
    colours <- c(col, col2)
    dots$col <- as.factor(dots$var2)
    levels(dots$col) <- colours
    mycols <- as.character(dots$col)
    # nbCols <- length(levels(as.factor(dots$var2)))
  }else{
    mycols <- rep(col, nrow(dots))
  }
  
  # CIRCLES
  if (symbols == "circles"){
    symbols(dots[, c("x", "y")], circles = dots$circleSize, bg = mycols,
            add = add,
            inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$circleSize
    varvect <- dots[,var]
    LegendCircSymbols(pos = legend.pos, legTitle = legend.title.txt,
                      legTitleCex = legend.title.cex,
                      legValuesCex = legend.values.cex,
                      varvect = varvect,
                      sizevect = sizevect,
                      breakval  = breakval,
                      col1 = col,
                      col2 = col2,
                      frame = legend.frame,
                      round = legend.values.rnd,
                      type = legend.style)
  }
  
  # SQUARES
  if (symbols == "squares"){
    symbols(dots[, c("x", "y")], squares = dots$squareSize, bg = mycols,
            add = add, inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$squareSize
    varvect <- dots[,var]
    LegendSquaresSymbols(pos = legend.pos, legTitle = legend.title.txt,
                         legTitleCex = legend.title.cex, 
                         legValuesCex = legend.values.cex,
                         varvect = varvect,
                         sizevect = sizevect,
                         breakval = breakval, 
                         col1 = col, 
                         col2 = col2, 
                         frame = legend.frame, 
                         round = legend.values.rnd, 
                         type = legend.style)
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
    LegendHeightSymbols(pos = legend.pos, legTitle = legend.title.txt,
                        legTitleCex = legend.title.cex, 
                        legValuesCex = legend.values.cex,
                        varvect = varvect,
                        sizevect = sizevect,
                        breakval = breakval, 
                        col1 = col, 
                        col2 = col2, 
                        frame = legend.frame, 
                        round = legend.values.rnd, 
                        type = legend.style)
    
  }
  
}

