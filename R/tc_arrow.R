#' @title Plot a north arrow
#' @name tc_arrow
#' @eval my_params(c('pos'))
#' @param col arrow color
#' @param adjust object of class \code{sf} or \code{sfc} used to adjust the 
#' arrow to the real north
#' @importFrom sf st_crs st_as_sf st_coordinates st_transform
#' @export
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq)
#' tc_arrow(pos = "topright")
#' @export
tc_arrow <- function(pos = "topright", col, adjust) {
  if (missing(col)) {
    col <- .gmapsf$args$fg
  }
  
  # why commented, test?
  # op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  # on.exit(par(op))
  
  azim <- "N"
  theta <- 0
  mapExtent <- par("usr")

  xe <- mapExtent[1:2]
  ye <- mapExtent[3:4]
  inset <- strwidth("M", units = "user", cex = 1)
  
  if (is.numeric(pos) & length(pos) == 2) {
    xarrow <- pos[1]
    yarrow <- pos[2]
  } else {
    switch(pos,
           topleft = {
             xarrow <- xe[1] + inset * .5
             yarrow <- ye[2] - inset * 2
           },
           bottomright = {
             xarrow <- xe[2] - inset * 1.5
             yarrow <- ye[1] + inset * 2
           },
           topright = {
             xarrow <- xe[2] - inset * 1.5
             yarrow <- ye[2] - inset * 2
           },
           bottomleft = {
             xarrow <- xe[1] + inset * .5
             yarrow <- ye[1] + inset * 2
           },
           top = {
             xarrow <- xe[1] + diff(xe) / 2 - inset * .5
             yarrow <- ye[2] - inset * 2
           },
           bottom = {
             xarrow <- xe[1] + diff(xe) / 2 - inset * .5
             yarrow <- ye[1] + inset * 2
           },
           left = {
             xarrow <- xe[1] + inset * .5
             yarrow <- ye[1] + diff(ye) * 0.5 - inset
           },
           right = {
             xarrow <- xe[2] - inset * 1.5
             yarrow <- ye[1] + diff(ye) * 0.5 - inset
           }
    )
  }
  
  xx <- c(xarrow, xarrow + inset / 2, xarrow + inset)
  yy <- c(yarrow, yarrow + inset * 1.5, yarrow)
  
  if (!missing(adjust)) {
    xcrs <- st_crs(adjust)
    xp <- xarrow + inset / 2
    yp <- yarrow + inset * 1.5
    A <- st_as_sf(
      x = data.frame(X = xp, Y = yp), coords = c("X", "Y"),
      crs = xcrs, remove = FALSE
    )
    B <- st_as_sf(
      x = data.frame(st_coordinates(A) + c(0, -100000)),
      coords = c("X", "Y"), crs = xcrs, remove = FALSE
    )
    Ap <- st_transform(A, 4326)
    Cp <- st_as_sf(
      x = data.frame(st_coordinates(Ap) + c(0, -2)),
      coords = c("X", "Y"), crs = 4326, remove = FALSE
    )
    C <- st_transform(Cp, xcrs)
    C[, c("X", "Y")] <- st_coordinates(C)
    DeltaXB <- B$X - A$X
    DeltaYB <- B$Y - A$Y
    DeltaXC <- C$X - A$X
    DeltaYC <- C$Y - A$Y
    VX <- c(DeltaXB, DeltaYB)
    VY <- c(DeltaXC, DeltaYC)
    theta <- acos(sum(VX * VY) / (sqrt(sum(VX * VX)) * sqrt(sum(VY * VY))))
    theta <- sign(C$X - B$X) * theta
    nc <- rot(A, c(xx[1], yy[1]), theta)
    xx[1] <- nc[1]
    yy[1] <- nc[2]
    nc <- rot(A, c(xx[3], yy[3]), theta)
    xx[3] <- nc[1]
    yy[3] <- nc[2]
  }

  
  
  polygon(xx, yy, col = col, border = col, xpd = TRUE)
  text(
    x = xx[1] + (xx[3] - xx[1]) / 2, y = yarrow, labels = azim,
    adj = c(0.5, 1.5), cex = 0.7, xpd = TRUE,
    font = 2, col = col, srt = theta * 180 / pi
  )
  

}


rot <- function(A, B, theta) {
  x <- cos(theta) * (B[1] - A$X) - sin(theta) * (B[2] - A$Y) + A$X
  y <- sin(theta) * (B[1] - A$X) + cos(theta) * (B[2] - A$Y) + A$Y
  return(c(x, y))
}
