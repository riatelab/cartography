#' @title Proportional Symbols Layer
#' @name propSymbolsLayer
#' @description Plot a proportional symbols layer.
#' @param spdf SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df data.frame; df contains the values to plot.
#' @param spdfid character; id field in spdf, default to the first column 
#' of the spdf data.frame. (optional)
#' @param dfid character; id field in df, default to the first column 
#' of df. (optional)
#' @param var character; name of the numeric field in df to plot.
#' @param symbols character; type of symbols, one of "circle", "square" or "bar".
#' @param col character; color of symbols.
#' @param col2 character; second color of symbols (see Details).
#' @param border character; color of polygon borders.
#' @param lwd numeric; borders width.
#' @param breakval numeric; breaking value (see Details).
#' @param k numeric; share of the map occupied by the biggest symbol (see Details).
#' @param fixmax numeric; value of the biggest symbol (see Details).
#' @param legend.pos character; position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt character; title of the legend.
#' @param legend.title.cex numeric; size of the legend title.
#' @param legend.values.cex numeric; size of the values in the legend.
#' @param legend.values.rnd numeric; number of decimal places of the values 
#' displayed in the legend.
#' @param legend.style character; either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details The breakval parameter allows to plot symbols of two 
#' colors: the first color (col) for values superior or equal to breakval,
#' second color (col2) for values inferior to breakval.
#' 
#' Two maps with the same spdf, k, and fixmax parameters will be comparable.
#' @return A plot is returned.
#' @export
#' @import sp
#' @examples
#' ## data("nuts2006")
#' ## Exemple 1
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
#'                  var = "pop2008", k = 0.01,
#'                  symbols = "square", col =  "#920000",
#'                  legend.pos = "right", 
#'                  legend.title.txt = "Total\npopulation (2008)", 
#'                  legend.style = "c")
#' 
#' ## Exemple 2
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                  var = "gdppps2008", k = 0.01,
#'                  symbols = "bar", col =  "#B00EF0",
#'                  legend.pos = "right", 
#'                  legend.title.txt = "GDP\nin Millions PPS (2008)", 
#'                  legend.style = "e")
#' 
#' ## Exemple 3
#' oldpar <- par(mfrow = c(1,2), mar = c(0,0,0,0))
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                  var = "birth_2008", k = 0.01,
#'                  fixmax = max(nuts0.df$birth_2008),
#'                  symbols = "circle", col =  "orange",
#'                  legend.pos = "right", 
#'                  legend.title.txt = "nb of births", 
#'                  legend.style = "e")
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                  var = "death_2008", k = 0.01,
#'                  symbols = "circle", col =  "pink",
#'                  fixmax = max(nuts0.df$birth_2008),
#'                  legend.pos = "right", 
#'                  legend.style = "e",
#'                  legend.title.txt = "nb of deaths")
#' par(oldpar)
#' 
#' ## Exemple 4
#' nuts0.df$balance <- nuts0.df$birth_2008-nuts0.df$death_2008
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                  var = "balance", k = 0.01,
#'                  symbols = "circle",
#'                  col = "orange", col2 = "green", breakval=0,
#'                  legend.pos = "right", 
#'                  legend.style = "c",
#'                  legend.title.txt = "Natural Balance\n(2008)")
propSymbolsLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                             symbols = "circle",
                             col = "#E84923", col2 = "#7DC437", border = "black", 
                             lwd = 1,
                             breakval = NULL,
                             k = 0.02, fixmax = NULL,
                             legend.pos = "bottomleft", legend.title.txt = var,
                             legend.title.cex = 0.8, legend.values.cex = 0.6,
                             legend.style = "c", legend.frame = FALSE,
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
    dots$col <- col2
    dots[dots[,var] >= breakval & !is.na(dots[,var]), "col"] <- col
    mycols <- as.character(dots$col)
    # nbCols <- length(levels(as.factor(dots$var2)))
  }else{
    mycols <- rep(col, nrow(dots))
  }
  
  if (add==FALSE){
    sp::plot(spdf, col = NA, border = NA)
  }
  
  
  # CIRCLES
  if (symbols == "circle"){
    symbols(dots[, c("x", "y")], circles = dots$circleSize, bg = mycols, 
            fg = border, lwd = lwd,
            add = TRUE,
            inches = FALSE, asp = 1	)
    sizevect <- dots$circleSize
    varvect <- dots[,var]
    if(legend.pos!="n"){
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
  }
  # SQUARES
  if (symbols == "square"){
    symbols(dots[, c("x", "y")], squares = dots$squareSize, bg = mycols,
            fg = border, lwd = lwd,
            add = TRUE, inches = FALSE, asp = 1)
    sizevect <- dots$squareSize
    varvect <- dots[,var]
    if(legend.pos!="n"){
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
  }
  
  #BARRES
  if (symbols == "bar"){
    width<-min((par()$usr[4] - par()$usr[3]) / 40, (par()$usr[2] - par()$usr[1]) / 40)
    tmp <- as.matrix(data.frame(width, dots$heightSize))
    dots$y2 <- dots$y + dots$heightSize / 2
    symbols(dots[,c("x","y2")], rectangles = tmp, add = TRUE, bg = mycols,
            fg = border, lwd = lwd,
            inches = FALSE, asp = 1)
    sizevect <- dots$heightSize
    varvect <- dots[,var]
    if(legend.pos!="n"){
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
}

