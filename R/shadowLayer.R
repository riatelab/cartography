#' @title Plot a Shadow Layer 
#' @name shadowLayer
#' @description Plot an invisible layer with the extent of a spatial object.  
#' @param x an sf object, a simple feature collection or a 
#' Spatial*DataFrame.
#' @param bg background color.
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' target <- mtq[30,]
#' shadowLayer(target, bg = "lightblue")
#' plot(st_geometry(mtq), add = TRUE, col = "gold2")
#' plot(st_geometry(target), add = TRUE, col = "red")
#' labelLayer(x = suppressWarnings(st_intersection(mtq, st_buffer(target, 2000))), 
#'            txt = "LIBGEO", halo = TRUE, cex = .9, r = .14, font = 2, 
#'            bg = "grey20", col= "white")
#' @export
shadowLayer <- function(x, bg){
  if (methods::is(x, 'Spatial')){
    x <- sf::st_as_sf(x)
  }
  # get the bounding box
  bb <- sf::st_bbox(x)
  # plot an empty plot
  plot(x = 1, type="n", xlim = bb[c(1,3)], ylim = bb[c(2,4)], 
       axes = FALSE, xlab = NA, ylab = NA, asp = TRUE)
  # background color
  if(!missing(bg)){
    bb2 <- par("usr")
    graphics::rect(bb2[1], bb2[3], bb2[2], bb2[4], col = bg, border = NA)
  }
}