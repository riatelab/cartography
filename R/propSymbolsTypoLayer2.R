propSymbolsTypoLayer2 <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                                  k = 0.02, fixmax = NULL, symbols = "circle",
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

