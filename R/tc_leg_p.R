#' Plot a legend for a proportional symbols map
#' @description This function plots a legend for proportional symbols.
#'
#' @param symbol type of symbols, 'circle' or 'square'
#' @param inches size of the biggest symbol (radius for circles, half width
#' for squares) in inches
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param val vector of values (at least min and max).
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param border color of the symbols borders
#' @param cex size of the legend; 2 means two times bigger
#' @param lwd width of the symbols borders
#' @param col color of the symbols
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @export
#' @import graphics
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' tc_leg_p(val = c(1,20,100), col = "red", inches = .3)
#' box()
tc_leg_p <- function(pos = "left",
                  val,
                  col,
                  inches,
                  symbol = "circle",
                  border,
                  lwd = .7,
                  title = "Title of the Legend",
                  title_cex = .8,
                  val_cex = .6,
                  val_rnd = 0,
                  frame = FALSE,
                  bg,
                  fg,
                  cex = 1){
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
  if (missing(border)) border <- fg
  
  
  val <- sort(val, decreasing = TRUE)
  valleg <- get_val_rnd(val = val, val_rnd = val_rnd)
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
    xy_symbols <- get_xy_s(
      x = xy_title$x,
      y = xy_title$y - inset / 2,
      val = val,
      inches = inches,
      symbol = symbol
    )
    xy_lines <- get_xy_lines(
      x = xy_symbols$x[1],
      y = xy_symbols$y,
      sizesi = xy_symbols$s,
      inset = inset / 4
    )
    xy_lab <- get_xy_lab_s(
      x = xy_lines$x1 + inset / 4,
      y = xy_symbols$y + xy_symbols$s,
      val = valleg,
      val_cex = val_cex
    )
    
    xy_rect <- get_xy_rect_s(
      xy_title = xy_title,
      xy_symbols = xy_symbols,
      xy_lines = xy_lines,
      xy_lab = xy_lab,
      inset = inset
    )
    
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
  text(xy_title$x,
       y = xy_title$y, labels = title, cex = title_cex,
       adj = c(0, 0), col = fg
  )
  dots <- data.frame(xy_symbols$x, xy_symbols$y)
  plot_symbols(
    symbol = symbol, dots = dots, sizes = xy_symbols$s, mycols = col,
    border = border, lwd = lwd, inches = inches
  )
  segments(
    x0 = xy_lines$x0, x1 = xy_lines$x1, y0 = xy_lines$y0,
    y1 = xy_lines$y1, col = border
  )
  text(xy_lab$x,
       y = xy_lab$y, labels = rev(valleg), cex = val_cex,
       adj = c(0, 0.5), col = fg
  )
  
  
  return(invisible(list(xy_rect = xy_rect, inset = inset, h = xy_title$h)))
}