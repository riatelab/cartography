# get prop symbols size and dim  from topleft corner
get_xy_s <- function(x, y, val, inches, symbol) {
  sizes <- get_size(
    var = val,
    inches = inches,
    val_max = max(val),
    symbol = symbol
  )
  sizesi <- xinch(sizes)
  
  x <- rep(x + sizesi[1], length(val))
  y <- y - sizesi[1] * 2 + sizesi
  h <- sizesi[1] * 2
  w <- h
  
  return(list(x = x, y = y, s = sizesi, h = h, w = w))
}

# lines from top of symbols to labels
get_xy_lines <- function(x, y, sizesi, inset) {
  x0 <- rep(x, length(sizesi))
  x1 <- rep(x + sizesi[1] + inset, length(sizesi))
  y0 <- y1 <- y + sizesi
  return(list(x0 = x0, x1 = x1, y0 = y0, y1 = y1, w = x1[1] - x0[1] + inset))
}

# get labels position
get_xy_lab_s <- function(x, y, val, val_cex) {
  w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
  y <- rev(y)
  return(list(x = x, y = y, w = w))
}

# box around the legend
get_xy_rect_s <- function(xy_title, xy_symbols, xy_lines, xy_lab, inset) {
  xy_leg <- list(
    xleft = xy_title$x,
    ybottom = xy_title$y - inset / 2 - xy_symbols$h,
    xright =
      xy_title$x +
      max(
        xy_title$w,
        xy_symbols$h / 2 +
          xy_lines$w +
          xy_lab$w
      ),
    ytop = xy_title$y + xy_title$h
  )
  xy_leg
}