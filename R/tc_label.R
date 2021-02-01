#' @title Plot labels
#' @description Put labels on a map.
#' @name tc_label
#' @eval my_params(c('x', 'var'))
#' @param col labels color
#' @param cex labels cex
#' @param ... further \link{text} arguments.
#' @param bg halo color
#' @param r width of the halo
#' @param overlap if FALSE, labels are moved so they do not overlap.
#' @param halo If TRUE, then a 'halo' is printed around the text and additional 
#' arguments bg and r can be modified to set the color and width of the halo.
#' @param lines if TRUE, then lines are plotted between x,y and the word, 
#' for those words not covering their x,y coordinate
#' @export
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq)
#' tc_label(x = mtq, var = "LIBGEO", halo = TRUE, cex = 0.8,
#'          overlap = FALSE, lines = FALSE)
tc_label <- function(x, var,
                     col,
                     cex = 0.7, overlap = TRUE, 
                     lines = TRUE, 
                     halo = FALSE, 
                     bg, 
                     r = 0.1, ...){
  # margins mgmt
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  
  if(missing(col)){
    col <- .gmapsf$args$fg
  }
  if(missing(bg)){
    bg <- .gmapsf$args$bg
  }
  words <- x[[var]]
  cc <- sf::st_coordinates(sf::st_centroid(
    x = sf::st_geometry(x),
    of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON"))
  ))
  
  if(nrow(x) == 1){
    overlap <- TRUE
  }
  
  if (!overlap){
    x <- unlist(cc[,1])
    y <- unlist(cc[,2])
    lay <- wordlayout(x,y,words,cex)
    
    if(lines){
      for(i in 1:length(x)){
        xl <- lay[i,1]
        yl <- lay[i,2]
        w <- lay[i,3]
        h <- lay[i,4]
        if(x[i]<xl || x[i]>xl+w ||
           y[i]<yl || y[i]>yl+h){
          points(x[i],y[i],pch=16,col=col,cex=.5)
          nx <- xl+.5*w
          ny <- yl+.5*h
          lines(c(x[i],nx),c(y[i],ny), col=col, lwd = 1)
        }
      }
    }
    cc <- matrix(data = c(lay[,1]+.5*lay[,3],  lay[,2]+.5*lay[,4]), 
                 ncol = 2, byrow = FALSE)
  }
  if (halo){
    shadowtext(x = cc[,1], y = cc[,2], labels = words,
               cex = cex, col = col, bg = bg, r = r, ...)
  }else{
    text(x = cc[,1], y = cc[,2], labels = words, cex = cex, col = col, ...)
  }
}
