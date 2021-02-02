#' Plot a legend for a proportional lines map
#' @description This function plots a legend for proportional lines.
#'
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param lwd width of the largest line
#' @param val vector of values (at least min and max).
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param cex size of the legend; 2 means two times bigger
#' @param col color of the lines
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @export
#' @import graphics
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' tc_leg_pl(lwd = 20, val = c(5,10,50,100))
tc_leg_pl <- function(pos = "left",
                      val,
                      lwd,
                      col = "tomato4",
                      title = "Title of the Legend",
                      title_cex = .8,
                      val_cex = .6,
                      val_rnd = 0,
                      frame = FALSE,
                      bg,
                      fg,
                      cex = 1){
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
  if(missing(bg)) bg <- par("bg")
  if (missing(fg)) fg <- par("fg")

  
  w <- inset
  h <- inset / 1.5
  n <- length(val)
  
  # lwd = 5
  # val <- c(1,5,10)
  # 
  val <- sort(val)
  lwds <- lwd * val / max(val)
  val <- get_val_rnd(val = val, val_rnd = val_rnd)
  val <- rev(val)
  lwds <- rev(lwds)

  xy_leg <- NULL
    
    
  while (TRUE) {
    if (length(pos) == 2 & is.numeric(pos)) {
      xy_leg <- pos
    }
    xy_title <- get_xy_title(
      x = xy_leg[1],
      y = xy_leg[2],
      title = title,
      title_cex = title_cex
    )
    xy_box <- get_xy_box_t(
      x = xy_title$x,
      y = xy_title$y - inset / 2,
      n = n,
      w = w,
      h = h,
      inset = inset / 2
    )
   
    xy_box_lab <- get_xy_box_lab_t(
      x = xy_box$xright[n] + inset / 4,
      y = xy_box$ytop[1],
      h = h,
      val = val,
      val_cex = val_cex,
      inset = inset / 2
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
  
  if (frame) {
    rect(
      xleft = xy_rect[[1]] - insetf / 4,
      ybottom = xy_rect[[2]] - insetf / 4,
      xright = xy_rect[[3]] + insetf / 4,
      ytop = xy_rect[[4]] + insetf / 4,
      col = bg, border = fg, lwd = .7, xpd = T
    )
  }
  text(xy_title$x,
       y = xy_title$y, labels = title, cex = title_cex,
       adj = c(0, 0), col = fg
  )
  

  segments(x0 = xy_box[[1]], 
           y0 = xy_box[[2]] + (xy_box[[4]] - xy_box[[2]]) / 2 , 
           x1 = xy_box[[3]], 
           y1 = xy_box[[2]] + (xy_box[[4]] - xy_box[[2]]) / 2, 
           col = col,
           lwd = lwds, lend = 1
  )
  
  text(xy_box_lab$x,
       y = xy_box_lab$y, labels = val, cex = val_cex,
       adj = c(0, 0.5), col = fg
  )
  return(invisible(list(xy_rect = xy_rect, inset = inset, h = xy_title$h)))
}
