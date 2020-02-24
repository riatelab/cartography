#' @title Plot a Raster Object
#' @description Plot a raster object over a map. It can be used to plot tiles 
#' from getTiles or images from getPngLayer. 
#' @name tilesLayer
#' @aliases tilesLayer
#' @aliases pngLayer
#' @param x a RasterBrick object; \link{getPngLayer} and \link{getTiles} 
#' functions output these objects.
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
#' @seealso \link{getPngLayer}, \link{getTiles}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' 
#' \dontrun{
#' # Download the tiles, extent = Martinique
#' mtqOSM <- getTiles(x = mtq, type = "osm", crop = TRUE)
#' # Plot the tiles
#' tilesLayer(mtqOSM)
#' # Plot countries
#' plot(st_geometry(mtq), add=TRUE)
#' txt <- "© OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright"
#' mtext(text = txt, side = 1, adj = 0, cex = 0.7, font = 3)
#' }
#' 
#' # Local image
#' dirpng <- system.file("img/LogoMartinique.png", package = "cartography")
#' mask <- getPngLayer(mtq, dirpng, crop = TRUE, margin = 0.5)
#' par(mar = c(0,0,0,0))
#' ghostLayer(mtq)
#' pngLayer(mask, add = TRUE)
#'
#' \dontrun{
#' # Remote image
#' urlpng = "https://i.imgur.com/gePiDvB.png"
#' masksea <- getPngLayer(mtq, urlpng, mode = "wb", inverse = TRUE, margin = 0.5)
#' #Combine
#' par(mar = c(0,0,0,0))
#' ghostLayer(mtq)
#' pngLayer(mask, add = TRUE)
#' pngLayer(masksea, add = TRUE)
#' plot(st_geometry(mtq), border="orange", add=TRUE)
#' }
tilesLayer <- function(x, add = FALSE, ...) {
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

#' @rdname tilesLayer
#' @export
pngLayer <- tilesLayer