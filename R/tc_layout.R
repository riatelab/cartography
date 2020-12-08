#' @title Plot title, credits, scale and arrow
#' @description Plot a layout layer (title, credits, scalebar, north arrow).
#' @name tc_layout
#' @param title title of the map
#' @param credits credits
#' @param scale display a scale bar
#' @param arrow display an arrow
#' @param frame display a frame
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq) 
#' tc_layout()
tc_layout <- function(title = "Title of the map, year",
                      credits = "Authors & Sources",
                      scale = TRUE, arrow = TRUE, frame = FALSE) {
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  fg <- .gmapsf$args$fg
  
  if (title != "") {
    tc_title(txt = title)
  }
  
  if (credits != "") {
    tc_credits(txt = credits, pos = "bottomleft")
  }
  if (arrow) {
    tc_arrow()
  }
  if (scale) {
    tc_scale()
  }
  if (frame) {
    box(col = fg)
  }
  

}
