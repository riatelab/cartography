propSymbolsLayer2 <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                             k = 0.02, fixmax = NULL, breakval = NULL,
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
    symbols(dots[, 2:3], circles = dots$circleSize, bg = mycols,
            fg = border, lwd = lwd,
            add = TRUE,
            inches = FALSE, asp = 1	)
    sizevect <- dots$circleSize
    varvect <- dots[,var]
    if(legend.pos!="n"){
      legendCirclesSymbols(pos = legend.pos, title.txt = legend.title.txt,
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
  }
  # SQUARES
  if (symbols == "square"){
    symbols(dots[, 2:3], squares = dots$squareSize, bg = mycols,
            fg = border, lwd = lwd,
            add = TRUE, inches = FALSE, asp = 1)
    sizevect <- dots$squareSize
    varvect <- dots[,var]
    if(legend.pos!="n"){
      legendSquaresSymbols(pos = legend.pos, title.txt = legend.title.txt,
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
  }
  
  #BARRES
  if (symbols == "bar"){
    width<-min((par()$usr[4] - par()$usr[3]) / 40, (par()$usr[2] - par()$usr[1]) / 40)
    tmp <- as.matrix(data.frame(width, dots$heightSize))
    dots[,3] <- dots[,3] + dots$heightSize / 2
    symbols(dots[,2:3], rectangles = tmp, add = TRUE, bg = mycols,
            fg = border, lwd = lwd,
            inches = FALSE, asp = 1)
    sizevect <- dots$heightSize
    varvect <- dots[,var]
    if(legend.pos!="n"){
      legendBarsSymbols(pos = legend.pos, title.txt = legend.title.txt,
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
  }
}

