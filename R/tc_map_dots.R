#' @title Plot a dot dentity map
#' @name tc_map_dots
#' @description Plot a dot density map
#' @param x an sf object 
#' @param var name of the numeric variable to plot.
#' @param n one dot on the map represents n (in var units).
#' @param col color of the points.
#' @param pch symbol to use: \link{points}.
#' @param cex size of the symbols
#' @param type points allocation method: "random" or "regular" (see Details).
#' @param leg_title text in the legend.
#' @param leg_pos 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param leg_cex size of the legend text.
#' @param leg_frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details 
#' The type parameters is defined within the \link{st_sample} function.
#' @export
#' @examples
#' \dontrun{
#' mtq <- tc_import_mtq()
#' tc_init(mtq, shadow = TRUE, theme = "green")
#' tc_map(mtq, add = TRUE)
#' tc_map_dots(x = mtq,  var="POP", pch=20, col = "red4",n = 200)
#' tc_layout(title = "Population Distribution in Martinique, 2015")
#' }
tc_map_dots <- function(x, 
                      var,
                      n, 
                      pch = 1,
                      cex = .15,
                      type = "random",
                      col = "black",
                      leg_pos = "topright",
                      leg_title,
                      leg_cex = 0.6,
                      leg_frame = TRUE,
                      add = TRUE){
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  
  if (missing(n)){
    n <- round(min(x[[var]], na.rm = TRUE), 0) + 1
  }
  
  x <- x[!is.na(x[[var]]), var]
  x$ndots <- as.integer(x[[var]]/n)
  x <- x[x$ndots > 0,]
  
  if (add == FALSE){
    tc_init(x)
  }
  
  plot(sf::st_sample(x = x, size = x[["ndots"]], type = type, 
                     exact = TRUE), 
       pch = pch, cex = cex , col = col, add = TRUE)
  
  if(leg_pos !="n"){
    if (missing(leg_title)){
      leg_title <- paste0("1 dot represents ", format(n, scientific = FALSE),
                           " [in ",var," units]")
    }
    if (leg_frame != TRUE){
      bg <- NA
      fg <- NA
    }
    legend(legend = leg_title, cex = leg_cex, text.col = fg, col = col,
           pch = pch, pt.cex = cex,  x = leg_pos, box.col = fg,
           bg = bg)
  }
}
