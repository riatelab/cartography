#' @title Plot Tiles from Open Map Servers
#' @description Plot tiles from open map servers.
#' @name tilesLayer
#' @param x a RasterBrick object; the \link{getTiles} function outputs these 
#' objects.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @note This function is a wrapper for plotRGB from the 
#' raster package.
#' @export
#' @seealso \link{getTiles}
#' @examples
#' \dontrun{
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' # Download the tiles, extent = Martinique
#' mtqOSM <- getTiles(x = mtq, type = "osm", crop = TRUE)
#' # Plot the tiles
#' tilesLayer(mtqOSM)
#' # Plot countries
#' plot(st_geometry(mtq), add=TRUE)
#' txt <- "© OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright"
#' mtext(text = txt, side = 1, adj = 0, cex = 0.7, font = 3)
#' }
tilesLayer <- function(x, add = FALSE){
  if (add == FALSE) {
    plot.new()
    plot.window(xlim = c(x@extent@xmin, x@extent@xmax), 
                ylim = c(x@extent@ymin, x@extent@ymax), 
                xaxs = "i", yaxs = "i", asp = TRUE)
  }
  
  raster::plotRGB(x = x, 
                  interpolate = TRUE, 
                  maxpixels = raster::ncell(x), 
                  add  = TRUE)
}

