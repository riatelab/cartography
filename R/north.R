#' @title North Arrow
#' @description Plot a north arrow.
#' @name north
#' @param pos position of the north arrow. It can be one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates (c(x, y)).
#' @param south plot a south arrow instead
#' @param col arrow color.
#' @export
#' @seealso \link{layoutLayer}
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' box()
#' for (i in list("topleft", "top", "topright", "right", "bottomright", 
#'                "bottom", "bottomleft", "left", c(3502127, 4770427))){
#'   north(i)
#' }
#' 
#' plot(nuts0.spdf)
#' box()
#' for (i in list("topleft", "top", "topright", "right", "bottomright", 
#'                "bottom", "bottomleft", "left", c(3502127, 4770427))){
#'   north(i, south = TRUE)
#' }
north <- function(pos = "topright", col = "grey20", south = FALSE){
  azim <- "N"
  mapExtent <- par()$usr
  x <- mapExtent[1:2]
  y <- mapExtent[3:4]
  inset <- min(diff(x), diff(y)) / 40
  
  if (is.numeric(pos) & length(pos)==2){
    xarrow <- pos[1]
    yarrow <- pos[2]
  }else{
    switch(pos, 
           topleft={
             xarrow <- x[2] - inset * 1.5
             yarrow <- y[2] - inset * 2
           }, 
           bottomright={
             xarrow <- x[2] - inset * 1.5
             yarrow <- y[1] + inset * 2
           },  
           topright={
             xarrow <- x[1] + inset * .5
             yarrow <- y[2] - inset * 2
           },
           bottomleft={
             xarrow <- x[1] + inset * .5
             yarrow <- y[1] + inset * 2
           },
           top={
             xarrow <- x[1] + diff(x) / 2 -  inset * .5
             yarrow <- y[2] - inset * 2
           },
           bottom={
             xarrow <- x[1] + diff(x) / 2 - inset * .5
             yarrow <- y[1] + inset * 2
           },
           left={
             xarrow <- x[1] + inset * .5
             yarrow <- y[1] + diff(y) * 0.5 - inset 
           },
           right={
             xarrow <- x[2] - inset * 1.5
             yarrow <- y[1] + diff(y) * 0.5 - inset 
           } 
    )
  }
  
  if(south==FALSE){
    xx <- c(xarrow, xarrow + inset / 2, xarrow + inset)
    yy <- c(yarrow, yarrow + inset * 1.5, yarrow)
  }else{
    xx <- c(xarrow, xarrow + inset / 2, xarrow + inset * 1)
    yy <- c(yarrow + inset * 1.5,yarrow, yarrow + inset * 1.5)
    azim <- "S"
  }
  polygon(xx, yy, col = col, border = col)
  text(xarrow + inset * .5, yarrow, azim, adj = c(0.5, 1.5), cex = 0.8,
       font = 2, col = col)
}



