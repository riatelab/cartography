# get coordinates of boxes and total size of box extent
# for choro
get_xy_box_c <- function(x, y, n, w, h) {
  xleft <- rep(x, n)
  xright <- rep(x + w, n)
  ytop <- rep(NA, n)
  ybottom <- rep(NA, n)
  for (i in 1:n) {
    ytop[i] <- y - (i - 1) * h
    ybottom[i] <- ytop[i] - h
  }
  h <- ytop[1] - ybottom[n]
  w <- w
  return(list(
    xleft = unname(xleft),
    ybottom = unname(ybottom),
    xright = unname(xright),
    ytop = unname(ytop),
    h = h,
    w = w
  ))
}

# get the position of the labels
# choro
get_xy_box_lab_c <- function(x, y, h, val, val_cex) {
  n <- length(val)
  xc <- rep(x, n)
  yc <- rep(NA, n)
  for (i in 1:n) {
    yc[i] <- y - (i - 1) * h
  }
  h <- strheight(val[n], units = "user", cex = val_cex, font = 1)
  w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
  return(list(x = xc, y = yc, w = w, h = h))
}

