#' @title Plot Geotagged \code{*.png} Files
#' @description Plot \code{*.png} over a map.
#' @name pngLayer
#' @param x a RasterBrick object; the \link{getPngLayer} function outputs these 
#' objects.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @param bgalpha,interpolate,... See \code{\link[raster:plotRGB]{plotRGB}}
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @note This function is a wrapper for \code{\link[raster:plotRGB]{plotRGB}} from the raster package. 
#' The accuracy of the final plot would depend on the quality of the \code{*.png} file, 
#' the scale of \code{x} and the resolution setup of the graphic device.
#' @export
#' @seealso \link{getPngLayer}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' #Local file
#' dirpng <- system.file("img/LogoMartinique.png", package = "cartography")
#' mask <- getPngLayer(mtq, dirpng, crop = TRUE, margin = 0.5)
#' par(mar=c(0,0,0,0))
#' ghostLayer(mtq)
#' pngLayer(mask, add = TRUE)
#' 
#' \dontrun{
#' #Remote file
#' urlpng = "https://i.imgur.com/gePiDvB.png"
#' masksea <- getPngLayer(mtq, urlpng, mode = "wb", inverse = TRUE, margin = 0.5 )
#' #Combine
#' par(mar=c(0,0,0,0))
#' plot(st_geometry(mtq), col=NA, border = NA)
#' pngLayer(mask, add = TRUE)
#' pngLayer(masksea, add = TRUE)
#' plot(st_geometry(mtq), border="orange", add=TRUE)
#' }
#' 
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' par(mar = c(0, 0, 0, 0))
#' #Local file
#' dirpng = system.file("img/LogoMartinique.png", package = "cartography")
#' 
#' mask <- getPngLayer(mtq, dirpng, crop = TRUE, margin = 0.5)
#' 
#' #Remote file
#' urlpng = "https://i.imgur.com/gePiDvB.png"
#' par(mar=c(0,0,0,0))
#' masksea <- getPngLayer(mtq, urlpng, mode = "wb", inverse = TRUE, margin = 0.5 )
#' 
#' #Combine
#' plot(st_geometry(mtq), col=NA, border = NA)
#' pngLayer(mask, add = TRUE)
#' pngLayer(masksea, add = TRUE)
#' plot(st_geometry(mtq), border="orange", add=TRUE)

pngLayer <- function(x, add = FALSE, bgalpha = 0, interpolate = TRUE , ...) {
  raster::plotRGB(
    x = x,
    interpolate = interpolate,
    maxpixels = raster::ncell(x),
    add  = add,
    bgalpha = bgalpha,
    ...
  )
}
