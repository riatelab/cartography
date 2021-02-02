#' @title Plot Tiles
#' @description Plot a raster object over a map. It can be used to plot tiles 
#' from getTiles. 
#' @name tilesLayer
#' @param x a RasterBrick object; \link{getTiles} 
#' function outputs these objects.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @param ... bgalpha, interpolate, or other arguments passed to be passed to
#' \code{\link[raster:plotRGB]{plotRGB}}
#' @export
#' @seealso \link[maptiles]{plot_tiles}
#' @keywords internal
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' 
tilesLayer <- function(x, add = FALSE, ...) {
  
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::tilesLayer()",
                            with = "maptiles::tc_map_r()")  
  
  if (add == FALSE) {
    ext <- sf::st_bbox(x)
    plot.new()
    plot.window(xlim = ext[c(1,3)], ylim = ext[c(2,4)], 
                xaxs = "i", yaxs = "i", asp = TRUE)
  }
  ops <- list(...)
  ops$x <- x
  ops$add <- TRUE
  #Default opts
  ops$maxpixels <- ifelse(is.null(ops$maxpixels), raster::ncell(x), ops$maxpixels)
  ops$bgalpha <- ifelse(is.null(ops$bgalpha), 0, ops$bgalpha)
  ops$interpolate <- ifelse(is.null(ops$interpolate), TRUE, ops$interpolate)
  do.call(raster::plotRGB, ops)
}





#' @title Plot a Raster Object
#' @description Plot a raster object over a map. It can be used to plot images 
#' from getPngLayer. 
#' @name pngLayer
#' @param x a RasterBrick object; \link{getPngLayer}
#' function outputs these objects.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @param ... bgalpha, interpolate, or other arguments passed to be passed to
#' \code{\link[raster:plotRGB]{plotRGB}}
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @note This function is a wrapper for \code{\link[raster:plotRGB]{plotRGB}} 
#' from the raster package. The accuracy of the final plot depends on the 
#' quality of the \code{*.png} file, the scale of \code{x} and the resolution 
#' setup of the graphic device.
#' @export
#' @keywords internal
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' 
#' # Local image
#' dirpng <- system.file("img/logo.png", package = "cartography")
#' mask <- getPngLayer(mtq, dirpng, crop = TRUE, margin = 0.5)
#' par(mar = c(0,0,0,0))
#' ghostLayer(mtq)
#' pngLayer(mask, add = TRUE)
#'
pngLayer <- function(x, add = FALSE, ...) {
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::pngLayer()",
                            with = "maptiles::tc_map_r()")  
  
  
  if (add == FALSE) {
    ext <- sf::st_bbox(x)
    plot.new()
    plot.window(xlim = ext[c(1,3)], ylim = ext[c(2,4)], 
                xaxs = "i", yaxs = "i", asp = TRUE)
  }
  ops <- list(...)
  ops$x <- x
  ops$add <- TRUE
  #Default opts
  ops$maxpixels <- ifelse(is.null(ops$maxpixels), raster::ncell(x), ops$maxpixels)
  ops$bgalpha <- ifelse(is.null(ops$bgalpha), 0, ops$bgalpha)
  ops$interpolate <- ifelse(is.null(ops$interpolate), TRUE, ops$interpolate)
  do.call(raster::plotRGB, ops)
}
