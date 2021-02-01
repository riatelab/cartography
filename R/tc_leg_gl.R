#' @title Plot a legend for a graduated lines map
#' @description This function plots a legend for graduated lines maps.
#'
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left" or a vector of two coordinates in map units
#' (c(x, y))
#' @param lwd lines widths
#' @param val break labels
#' @param col lines color
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param cex size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @export
#' @import graphics
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' tc_leg_gl(lwd =  c(0.2,2,4,5,10), val =c(1,2,3,4,10.2,15.2))
tc_leg_gl <- function(pos = "topleft",val,
                     col = "tomato4",
                     lwd,
                     title = "Title of the Legend",
                     title_cex = .8,
                     val_cex = .6,
                     val_rnd = 2,
                     frame = FALSE,
                     bg,
                     fg,
                     cex = 1) {
  
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  # stop if the position is not valid
  positions <- c(
    "bottomleft", "left", "topleft", "top", "bottom",
    "bottomright", "right", "topright",
    "bottomleft1", "bottomright1", "bottom1",
    "bottomleft2", "bottomright2", "bottom2",
    "topright1", "topleft1", "top1",
    "topright2", "topleft2", "top2"
  )
  if (length(pos) == 1) {
    if (!pos %in% positions) {
      return(invisible())
    }
  }
  
  # default values
  insetf <- strwidth("MM", units = "user", cex = 1)
  inset <- insetf * cex
  if (missing(bg)) bg <- .gmapsf$args$bg
  if (missing(fg)) fg <- .gmapsf$args$fg

  
  w <- inset
  h <- inset / 1.5
  val <- get_val_rnd(val = val, val_rnd = val_rnd)
  val <- rev(val)
  lwd <- rev(lwd)
  n <- length(val) - 1
  pal <- rev(col)
  xy_leg <- NULL
  while (TRUE) {
    if (length(pos) == 2) {
      xy_leg <- pos
    }
    xy_title <- get_xy_title(
      x = xy_leg[1],
      y = xy_leg[2],
      title = title,
      title_cex = title_cex
    )
    xy_box <- get_xy_box_c(
      x = xy_title$x,
      y = xy_title$y - inset / 2,
      n = n,
      w = w,
      h = h
    )

    xy_box_lab <- get_xy_box_lab_c(
      x = xy_box$xright[n] + inset / 4,
      y = xy_box$ytop[1],
      h = h,
      val = val,
      val_cex = val_cex
    )

    xy_rect <- get_xy_rect2(
      xy_title = xy_title,
      xy_box = xy_box,
      xy_box_lab = xy_box_lab,
      inset = inset,
      w = w )
    if (!is.null(xy_leg)) {
      break
    }
    xy_leg <- get_pos_leg(
      pos = pos,
      xy_rect = unlist(xy_rect),
      inset = inset,
      xy_title = xy_title,
      frame = frame
    )
  }
  
  
  # Display
  if (frame) {
    rect(
      xleft = xy_rect[[1]] - insetf / 4,
      ybottom = xy_rect[[2]] - insetf / 4,
      xright = xy_rect[[3]] + insetf / 4,
      ytop = xy_rect[[4]] + insetf / 4,
      col = bg, border = fg, lwd = .7, xpd = T
    )
  }
  # title
  text(xy_title$x,
       y = xy_title$y, labels = title, cex = title_cex,
       adj = c(0, 0), col = fg
  )
  # boxes
  segments(x0 = xy_box[[1]], 
           y0 = xy_box[[2]] + (xy_box[[4]] - xy_box[[2]]) / 2 , 
           x1 = xy_box[[3]], 
           y1 = xy_box[[2]] + (xy_box[[4]] - xy_box[[2]]) / 2, 
           col = pal,
           lwd = lwd, lend = 1
  )
  # labels
  text(xy_box_lab$x,
       y = xy_box_lab$y, labels = val, cex = val_cex,
       adj = c(0, 0.5), col = fg
  )

  return(invisible(list(xy_rect = xy_rect, inset = inset, h = xy_title$h)))
}
