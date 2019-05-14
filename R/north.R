#' @title North Arrow
#' @description Plot a north arrow.
#' @name north
#' @param pos position of the north arrow. It can be one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)).
#' @param south plot a south arrow instead.
#' @param col arrow color.
#' @param x sf or sp object used to correct the north azimuth
#' @export
#' @seealso \link{layoutLayer}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' for (i in list("topleft", "top", "topright", "right", "bottomright", 
#'                "bottom", "bottomleft", "left", c(746368, 1632993))){
#'   north(i, south = TRUE)
#' }
north <- function(pos = "topright", col = "grey20", south = FALSE, x = NULL){
  azim <- "N"
  theta = 0
  mapExtent <- par()$usr
  xe <- mapExtent[1:2]
  ye <- mapExtent[3:4]
  inset <- min(diff(xe), diff(ye)) / 40
  if (is.numeric(pos) & length(pos)==2){
    xarrow <- pos[1]
    yarrow <- pos[2]
  }else{
    switch(pos, 
           topleft={
             xarrow <- xe[1] + inset * .5
             yarrow <- ye[2] - inset * 2
           }, 
           bottomright={
             xarrow <- xe[2] - inset * 1.5
             yarrow <- ye[1] + inset * 2
           },  
           topright={
             xarrow <- xe[2] - inset * 1.5
             yarrow <- ye[2] - inset * 2
           },
           bottomleft={
             xarrow <- xe[1] + inset * .5
             yarrow <- ye[1] + inset * 2
           },
           top={
             xarrow <- xe[1] + diff(xe) / 2 -  inset * .5
             yarrow <- ye[2] - inset * 2
           },
           bottom={
             xarrow <- xe[1] + diff(xe) / 2 - inset * .5
             yarrow <- ye[1] + inset * 2
           },
           left={
             xarrow <- xe[1] + inset * .5
             yarrow <- ye[1] + diff(ye) * 0.5 - inset 
           },
           right={
             xarrow <- xe[2] - inset * 1.5
             yarrow <- ye[1] + diff(ye) * 0.5 - inset 
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
  
  if(!missing(x)){
    xcrs <- sf::st_crs(x)
    xp <- xarrow + inset / 2
    yp <- yarrow + inset * 1.5
    A <- sf::st_as_sf(x = data.frame(X=xp, Y=yp), coords = c('X', 'Y'), 
                  crs = xcrs, remove = FALSE)
    B <- sf::st_as_sf(x = data.frame(sf::st_coordinates(A) + c(0,-100000)),
                  coords = c('X', 'Y'), crs = xcrs, remove = FALSE)
    Ap <- sf::st_transform(A, 4326)
    Cp <- sf::st_as_sf(x = data.frame(sf::st_coordinates(Ap) + c(0,-2)), 
                   coords = c('X', 'Y'), crs = 4326, remove=FALSE)
    C <- sf::st_transform(Cp,xcrs)
    C[,c("X","Y")] <- sf::st_coordinates(C)
    DeltaXB = B$X - A$X
    DeltaYB = B$Y - A$Y
    DeltaXC = C$X - A$X
    DeltaYC = C$Y - A$Y
    VX = c(DeltaXB, DeltaYB)
    VY = c(DeltaXC, DeltaYC)
    theta <- acos( sum(VX*VY) / ( sqrt(sum(VX * VX)) * sqrt(sum(VY * VY)) ) )
    theta <- sign(C$X - B$X) * theta
    nc <- rot(A,c(xx[1], yy[1]), theta)
    xx[1] <- nc[1]
    yy[1] <- nc[2]
    nc <- rot(A,c(xx[3], yy[3]), theta)
    xx[3] <- nc[1]
    yy[3] <- nc[2]
    
  }
  polygon(xx, yy, col = col, border = col)
  text(xx[1] + (xx[3] - xx[1]) / 2, yy[1], azim, adj = c(0.5, 1.5), cex = 0.8,
       font = 2, col = col, srt = theta * 180 / pi)
}


rot <- function(A,B, theta){
  x <- cos(theta) * (B[1] - A$X) - sin(theta) * (B[2] - A$Y) + A$X
  y <- sin(theta) * (B[1] - A$X) + cos(theta) * (B[2] - A$Y) + A$Y
  return(c(x,y))
}


