

# get coordinates of boxes and total size of box extent
# for symbols
get_xy_box_s <- function(x, y, n, w_cex, h_cex, inset) {
  xleft <- rep(x, n)
  xright <- x + w_cex
  ytop <- rep(NA, n)
  ybottom <- rep(NA, n)
  ytop[1] <- y
  ybottom[1] <- y - h_cex[1]
  for (i in 2:n) {
    ytop[i] <- ybottom[i - 1] - inset
    ybottom[i] <- ytop[i] - h_cex[i]
  }
  h <- ytop[1] - ybottom[n]
  w <- max(xright) - x
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
# symbols
get_xy_box_lab_s <- function(x, y, h, val, val_cex, inset) {
  n <- length(val)
  xc <- rep(x, n)
  yc <- rep(NA, n)
  for (i in 1:n) {
    yc[i] <- y - (i - 1) * h[i] - h[i] / 2 - (i - 1) * inset
  }
  w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
  return(list(x = xc, y = yc, w = w))
}

