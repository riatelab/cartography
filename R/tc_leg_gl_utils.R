get_xy_rect2 <- function(xy_title, xy_box, 
                         xy_box_lab,  
                         inset, w) {
  xy_leg <- list(
    xleft = xy_title$x,
    ybottom =
      xy_title$y - inset / 2 -
      xy_box$h,
    xright = xy_title$x +
      max(
        xy_title$w,
        w + inset / 4 + xy_box_lab$w
      ),
    ytop = xy_title$y + xy_title$h
  )
  xy_leg
}