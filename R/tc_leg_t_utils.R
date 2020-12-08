# get the position of the labels
# typo
get_xy_box_lab_t <- function(x, y, h, val, val_cex, inset) {
  n <- length(val)
  xc <- rep(x, n)
  yc <- rep(NA, n)
  for (i in 1:n) {
    yc[i] <- y - (i - 1) * h - h / 2 - (i - 1) * inset
  }
  w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
  return(list(x = xc, y = yc, w = w))
}


# get coordinates of boxes and total size of box extent
# for typo
get_xy_box_t <- function(x, y, n, w, h, inset) {
  xleft <- rep(x, n)
  xright <- rep(x + w, n)
  ytop <- rep(NA, n)
  ybottom <- rep(NA, n)
  for (i in 1:n) {
    ytop[i] <- y - (i - 1) * h - (i - 1) * inset
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
