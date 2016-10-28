#' @title Scale Bar
#' @description Plot a scale bar.
#' @name barscale
#' @param size size of the scale bar in kilometers. If set to NULL, an 
#' automatic scale bar is displayed (1/10 of the map width).
#' @param lwd width of the scale bar.
#' @param cex cex of the text.
#' @param pos position of the legend, default to the bottom right corner of the map. 
#' A vector of two coordinates (c(x, y) is possible.
#' @param style style of the legend, either "pretty" or "oldschool". The 
#' "oldschool" style only uses the "size" parameter.   
#' @export
#' @seealso \link{layoutLayer}
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' barscale(size = 1000)
#' barscale(size = 500, lwd = 3, cex = .9, pos = c(3553000, 1449000))
#' 
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' barscale(style = "oldschool")
barscale <- function(size = NULL, lwd = 1.5, cex = 0.6, pos = NULL, style="pretty"){
  # size = 10
  mapExtent <- par()$usr
  x <- mapExtent[1:2]
  y <- mapExtent[3:4]
  inset <- min(diff(x), diff(y)) / 40
  
  # default scale
  if(is.null(size)){
    size <- diff(x)/10
    size <- signif(size, digits = 0)
  }else{
    # km to m 
    size <- size * 1000
  }
  
  # label
  labelscale <- paste(size / 1000, "km", sep =" ")
  
  # xy pos
  xscale <- x[2] - inset * 0.5 - size
  yscale <- y[1] + inset
  
  if(!is.null(pos)){
    if(is.numeric(pos) & length(pos)==2){
      xscale <- pos[1]
      yscale <- pos[2]
    }
  }
  
  
  switch(EXPR = style,
         pretty = {
           segments(xscale, yscale, x1 = xscale + size, yscale, lwd = lwd)
           text(xscale + (size / 2), yscale + inset / 10,
                paste(labelscale,"\n",sep=""), cex = cex)
         }, 
         oldschool = {
           rect(x[2] - size - inset/2, y[1]+inset, x[2]-inset/2, y[1]+(y[2]-y[1])/200+inset/2,
                col = "black", border = "black")
           rect(x[2] - size - inset/2, y[1]+inset, x[2]-inset/2-size/2, 
                y[1]+(y[2]-y[1])/200+inset/2, col = "white", border = "black")
           rect(x[2] - size - inset/2, y[1]+inset, x[2]-inset/2-size+
                  size/4, y[1]+(y[2]-y[1])/200+inset/2, col = "black", border = "black")
           rect(x[2] - size / 4 - inset/2, y[1]+inset, x[2]-inset/2, y[1]+(y[2]-y[1])/200+
                  inset/2, col = "white", border = "black")
           text(x[2] - size / 2 - inset/2,y[1]+(y[2]-y[1])/200+inset,
                paste(labelscale,"\n",sep=""),cex=0.6)
         })
}




