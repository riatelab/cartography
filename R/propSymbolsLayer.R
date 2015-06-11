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
#' @details The (\code{breakval}) parameter allows to plot symbols of two colors: the first color (\code{col}) for values superior of equal to breakval,
#' second color (\code{col2}) for values inferior to breakval.
#' @export
#' @import sp
#' @examples
#' data("TNdeleg")
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

  #   propSymbolsLegendLayer(pos = pos, title = title, varname = var,
  #                          sizevect = sizevect, varvect = varvect,
  #                          col = col, col2 = col2, symbols = symbols,
  #                          nbCols = nbCols, breakval = breakval)







}



#
#
# propSymbolsLegendLayer <- function(pos = pos, title = title, varname = var,
#                                    sizevect = sizevect, varvect = varvect,
#                                    col = col, col2 = col2, symbols = symbols,
#                                    nbCols = nbCols, breakval = breakval){
#
#   if(is.null(title)){
#     title <- varname
#   }
#
#   # position of le legend ---------------------------------
#   x1 <- par()$usr[1]
#   x2 <- par()$usr[2]
#   y1 <- par()$usr[3]
#   y2 <- par()$usr[4]
#   yextent <- (y2 - y1) / 3
#   xextent <- (x2 - x1) / 3
#   delta <- min((y2 - y1) / 40, (x2 - x1) / 40)
#
#   coords <- data.frame(pos="topleft",x=x1+delta/2,y=y2-delta/2)
#   coords <- rbind(coords,data.frame(pos="top",
#                                     x=x1+xextent+delta/2,y=y2-delta/2))
#   coords <- rbind(coords,data.frame(pos="topright",
#                                     x=x1+xextent*2+delta/2,y=y2-delta/2))
#   coords <- rbind(coords,data.frame(pos="left",
#                                     x=x1+delta/2,y=y2-yextent-delta/2))
#   coords <- rbind(coords,data.frame(pos="right",
#                                     x=x1+xextent*2+delta/2,y=y2-yextent-delta/2))
#   coords <- rbind(coords,data.frame(pos="bottomleft",
#                                     x=x1+delta/2,y=y1+yextent+delta/2))
#   coords <- rbind(coords,data.frame(pos="bottom",
#                                     x=x1+xextent+delta/2,y=y1+yextent+delta/2))
#   coords <- rbind(coords,data.frame(pos="bottomright",
#                                     x=x1+xextent*2+delta/2,y=y1+yextent+delta/2))
#
#   l<-NULL
#   l$x <- coords[coords$pos == pos, "x"]
#   l$y <- coords[coords$pos == pos, "y"]
#
#   rLeg <- quantile(sizevect,c(1,0.90,0.50,0),type=1,na.rm = TRUE)
#   rVal <- quantile(varvect,c(1,0.90,0.50,0),type=1,na.rm = TRUE)
#
#   colours <- c(col,col2)
#
#   # CIRCLES
#   if(symbols == "circles"){
#     text(x=l$x,y=l$y,title,adj=c(0,1),cex=0.6)
#     xpos <- (l$x+rLeg[1]-rLeg/2)
#     ypos <- l$y+rLeg-rLeg[1]*2
#     ypos <- ypos-strheight(title,cex = 0.6)-delta
#     symbols(x = rep(l$x+rLeg[1],4),y=ypos,circles=rLeg,add=T,bg=col,inches=FALSE)
#     text(x=rep(l$x+rLeg[1],4)+rLeg[1]*1.2,y=(l$y+(2*rLeg)-rLeg[1]*2-delta-
#                                                strheight(title,cex = 0.6)),
#          rVal,cex=0.5,srt=0,adj=0)
#     for (i in 1:4){  segments (l$x+rLeg[1],
#                                (l$y+(2*rLeg[i])-rLeg[1]*2-delta-
#                                   strheight(title,cex = 0.6)),
#                                l$x+rLeg[1]+rLeg[1]*1.1,
#                                (l$y+(2*rLeg[i])-rLeg[1]*2-delta-
#                                   strheight(title,cex = 0.6)))
#     }
#
#     if (nbCols == 2){
#       tmp <- c ((x2-x1), (y2-y1))
#       size <- max(tmp)/50
#       symbols(x=rep(l$x+rLeg[1],4),y=ypos,circles=rLeg,add=T,bg="#CCCCCC",inches=FALSE)
#       for (i in 1:4){  segments (l$x+rLeg[1],(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(title,cex = 0.6)),l$x+rLeg[1]+rLeg[1]*1.1,(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(title,cex = 0.6)))}
#       rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]-delta, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]-delta*2,col=col)
#       text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]-delta-delta/4,paste("< ",breakval),adj=c(0,1),cex=0.5)
#       rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]-delta*2, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]-delta*3,col=col2)
#       text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]-delta*2-delta/4,paste("> ",breakval),adj=c(0,1),cex=0.5)
#     }
#   }
#
#   # SQUARES
#   if(symbols == "squares"){
#     text(x=l$x,y=l$y,title,adj=c(0,1),cex=0.6)
#     xpos<- (l$x+rLeg[1]-rLeg/2)
#     ypos <-l$y+rLeg/2-rLeg[1]
#     ypos<-ypos-strheight(title,cex = 0.6)-delta
#     symbols(x=xpos,y=ypos,squares=rLeg,add=TRUE,bg=col,inches=FALSE)
#     text(x=l$x+rLeg[1]*1.2,y=ypos+rLeg/2,rVal,cex=0.3,srt=0,adj=0)
#     for (i in 1:4){  segments (l$x+rLeg[1],ypos+rLeg/2,l$x+rLeg[1]*1.1,ypos+rLeg/2)}
#
#
#     if (nbCols == 2){
#       symbols(x=xpos,y=ypos,squares=rLeg,add=TRUE,bg="#CCCCCC",inches=FALSE)
#       rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]/2-delta, xpos[1]-rLeg[1]/2+delta, ypos[1]-rLeg[1]/2-delta*2,col=col)
#       text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]/2-delta-delta/4,paste("< ",breakval),adj=c(0,1),cex=0.5)
#       rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]/2-delta*2, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]/2-delta*3,col=col2)
#       text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]/2-delta*2-delta/4,paste("> ",breakval),adj=c(0,1),cex=0.5)
#     }
#   }
#
#   # BARRES
#   if(symbols == "height"){
#     text(x=l$x,y=l$y,title,adj=c(0,1),cex=0.6)
#     #tmp <- c ((bbox(object@geom)[3]-bbox(object@geom)[1]), (bbox(object@geom)[4]-bbox(object@geom)[2]))
#     width<-delta
#     tmp<-as.matrix(data.frame(width,rLeg))
#     symbols(x=rep(l$x+width/2,4),y=l$y+rLeg/2-rLeg[1]-strheight(title,cex = 0.6)-
#               delta,rectangles=tmp,add=TRUE,bg=col,inches=FALSE)
#     for (i in 1:4){  segments (l$x+width,l$y+rLeg[i]-rLeg[1]-strheight(title,cex = 0.6)-
#                                  delta,l$x+width*2,l$y+rLeg[i]-rLeg[1]-strheight(title,cex = 0.6)-delta)}
#     text(x=l$x+2*width*1.2,y=l$y+rLeg-rLeg[1]-strheight(title,cex = 0.6)-delta,rVal,cex=0.3,srt=0,adj=0)
#
#     if (nbCols==2){
#
#       symbols(x=rep(l$x+width/2,4),y=l$y+rLeg/2-rLeg[1]-strheight(title,cex = 0.6)-delta,rectangles=tmp,add=TRUE,bg="#CCCCCC",inches=FALSE)
#       rect(l$x, l$y-rLeg[1]-delta*2-strheight(title,cex = 0.6), l$x+delta, l$y-delta*3-rLeg[1]-strheight(title,cex = 0.6),col=col)
#       text(x=l$x+delta*1.2,y=l$y-rLeg[1]-delta*2-delta/4-strheight(title,cex = 0.6),paste("< ",breakval),adj=c(0,1),cex=0.5)
#       rect(l$x, l$y-rLeg[1]-delta*3-strheight(title,cex = 0.6), l$x+delta, l$y-delta*4-rLeg[1]-strheight(title,cex = 0.6),col=col2)
#       text(x=l$x+delta*1.2,y=l$y-rLeg[1]-delta*3-delta/4-strheight(title,cex = 0.6),paste("> ",breakval),adj=c(0,1),cex=0.5)
#     }
#   }
#
# }
#
#
#
#
#
