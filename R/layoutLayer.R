#### Methods wrappers
#' @title Layout Layer
#' @description Plot a layout layer.
#' @name layoutLayer
#' @param title title of the map.
#' @param sources sources of the map (or something else).
#' @param author author of the map (or something else).
#' @param scale size of the scale in kilometers. If set to NULL, no scale is 
#' displayed, if set to 0 an automatic scale is displayed (1/10 of the map width).
#' @param frame wheither displaying a frame (TRUE) or not (FALSE).
#' @param col color of the frame border.
#' @param coltitle color of the title.
#' @param bg color of the frame background.
#' @param north wheither displaying a Noth arrow (TRUE) or not (FALSE).
#' @param south wheither displaying a South arrow (TRUE) or not (FALSE).
#' @param extent a SpatialPolygonsDataFrame or a SpatialPointsDataFrame; set the 
#' extent of the frame to the one of a Spatial object. (optional)
#' @details If extent is not set, plot.new has to be called first.
#' @export
#' @seealso \link{labelLayer}
#' @examples
#' data("nuts2006")
#' # Example 1
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Layout plot
#' layoutLayer()
#' 
#' # Example 2
#' layoutLayer(scale = NULL,col = NA, coltitle = "black",
#'             sources = "", author = "",
#'             frame = FALSE, bg = "#A6CAE0",
#'             south = TRUE, extent = nuts0.spdf)
#' plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
#' plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=1, add=TRUE)
layoutLayer <- function(title = "Title of the map, year",
                        sources = "Source(s)", author = "Author(s)",
                        col = "black", coltitle = "white", bg = NULL,
                        scale = 0, frame = TRUE, north = FALSE, 
                        south = FALSE,
                        extent = NULL){
  
    if (!is.null(extent)){
      sp::plot(extent, border = NA, col = NA, add = FALSE)
      mapExtent <- par()$usr
    }else {
      mapExtent <- par()$usr
    }

  x1 <- mapExtent[1]
  x2 <- mapExtent[2]
  y1 <- mapExtent[3]
  y2 <- mapExtent[4]
  yextent <- (y2 - y1) / 3
  xextent <- (x2 - x1) / 3
  delta <- min((y2 - y1) / 40, (x2 - x1) / 40)
  
  # FRAME
  if(frame == TRUE){colf <- col}else{colf <- NA}
  rect(x1, y1, x2, y2, border = colf, col = bg)
  
  # SCALE
  if (!is.null(scale)){
    if(scale == 0){
      scalesize <- (x2-x1)/10
      scalesize <- signif(scalesize, digits = 0)
    }
    if(scale != 0){scalesize <- scale * 1000}
    labelscale <- paste(scalesize / 1000,"km", sep = " ")
    rect(x2 - scalesize - delta/2, y1+delta, x2-delta/2, y1+(y2-y1)/200+delta/2,
         col = "black", border = "black")
    rect(x2 - scalesize - delta/2, y1+delta, x2-delta/2-scalesize/2,
         y1+(y2-y1)/200+delta/2, col = "white", border = "black")
    rect(x2 - scalesize - delta/2, y1+delta, x2-delta/2-scalesize+
           scalesize/4, y1+(y2-y1)/200+delta/2, col = "black", border = "black")
    rect(x2 - scalesize / 4 - delta/2, y1+delta, x2-delta/2, y1+(y2-y1)/200+
           delta/2, col = "white", border = "black")
    text(x2 - scalesize / 2 - delta/2,y1+(y2-y1)/200+delta,
         paste(labelscale,"\n",sep=""),cex=0.6)
  }
  # NORTH
  if(north==T){
    xarrow<-x2-delta*1.5
    yarrow <- y2-delta*2
    xx <- c(xarrow,xarrow + delta / 2, xarrow + delta * 1)
    yy <- c(yarrow, yarrow + delta * 1.5, yarrow)
    polygon(xx, yy, col = "#DDDDDD", border = "#DDDDDD")
    text(xarrow+delta*.5,yarrow,"N",adj=c(0.5,1.5),cex=0.8,font=2,col="#DDDDDD")
  }
  
  if(south==T){
    xarrow <- x2 - delta * 1.5
    yarrow <- y2 - delta * 2
    xx <- c(xarrow, xarrow + delta / 2, xarrow + delta * 1)
    yy <- c(yarrow + delta * 1.5,yarrow, yarrow + delta * 1.5)
    polygon(xx, yy, col = "#DDDDDD", border = "#DDDDDD")
    text(xarrow+delta*.5,yarrow,"S",adj=c(0.5,1.5),cex=0.8,font=2,col="#DDDDDD")
  }
  
  
  # TITLE
  size<-0.8
  par(xpd = TRUE)
  rect(x1, y2, x2, y2+delta+strheight(title,cex = size),border = col, col = col)
  text(x1+delta/2,y2+delta/2,title,adj=c(0,0),cex=size, col = coltitle,font=2)
  par(xpd = FALSE)
  
  
  # SOURCES
  text(x1+delta/2,y1+delta/2,paste(sources,author,sep="\n"),adj=c(0,0),
       cex=0.6,font=3)
  
  
}


