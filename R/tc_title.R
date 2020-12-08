#' @title Plot a title
#' @param txt title text
#' @param pos position, one of 'left', 'center', 'right'
#' @param tab if TRUE the title is displayed as a 'tab'
#' @param bg background of the title
#' @param fg foreground of the title
#' @param cex cex of the title
#' @param font font of the title
#' @param line number of lines used for the title
#' @param inner if TRUE the title is displayed inside the plot area.
#' @eval my_params("xfull")
#' @export
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq)
#' tc_title()
tc_title <- function(txt = "Map Title, Year", pos, tab,
                     bg, fg, cex, line, font,
                     inner, x) {
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  
  ar <- .gmapsf$args
  if (missing(tab)) tab <- ar$tab
  if (missing(pos)) pos <- ar$pos
  if (missing(inner)) inner <- ar$inner
  if (missing(line)) line <- ar$line
  if (missing(cex)) cex <- ar$cex
  if (missing(font)) font <- ar$font
  if (missing(bg)) bg <- ar$fg
  if (missing(fg)) fg <- ar$bg
  
  # size refs
  pu <- par("usr")
  hbox <- line * 0.2 * xinch(1)
  inset <- strwidth("M", units = "user", cex = 1) / 2
  wtitle <- strwidth(txt, units = "user", cex = cex, font = font)
  htitle <- strheight("M", units = "user", cex = cex, font = font)
  
  
  # compute rect coord
  pw <- pu[2] - pu[1]
  pb <- pu[c(1, 3, 2, 4)]
  pb <- switch(pos,
               left = {
                 pb[3] <- pu[1] + inset + wtitle + inset
                 pb
               },
               right = {
                 pb[1] <- pu[2] - (inset + wtitle + inset)
                 pb[3] <- pu[2]
                 pb
               },
               center = {
                 pb[1] <- pu[1] + (pw / 2) - (wtitle / 2) - inset
                 pb[3] <- pb[1] + wtitle + inset + inset
                 pb
               }
  )
  # adjust box coord if inner
  if (inner) {
    pb[2] <- pu[4] - hbox
    pb[4] <- pu[4]
  } else {
    pb[2] <- pu[4]
    pb[4] <- pu[4] + hbox
  }
  # title coord
  pt <- c(pb[1] + inset, pb[2] + (hbox - htitle) / 2)
  
  # adjust box
  if (tab == FALSE) {
    pb[c(1, 3)] <- pu[1:2]
  }
  
  # display rect
  rect(
    xleft = pb[1],
    ybottom = pb[2],
    xright = pb[3],
    ytop = pb[4],
    col = bg,
    xpd = TRUE,
    border = bg
  )
  # display title
  text(
    x = pt[1],
    y = pt[2],
    labels = txt, adj = c(0, 0),
    cex = cex, col = fg,
    font = font, xpd = TRUE
  )
  
  
  if (missing(x)) {
    x <- NULL
  }
  return(invisible(x))
}
