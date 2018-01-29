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
#' library(sp)
#' data("nuts2006")
#' # extract Denmark
#' spdf <- nuts0.spdf[nuts0.spdf$id=="DK",]
#' # Download the tiles, extent = Denmark
#' den <- getTiles(spdf = spdf, type = "osm", crop = TRUE)
#' class(den)
#' # Plot the tiles
#' tilesLayer(den)
#' 
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' # Download the tiles, extent = Martinique
#' mtqOSM <- getTiles(x = mtq, type = "osm", crop = TRUE)
#' # Plot the tiles
#' tilesLayer(mtqOSM)
#' # Plot countries
#' plot(st_geometry(mtq), add=TRUE)
#' # Map tiles sources
#' mtext(text = "Map data © OpenStreetMap contributors, under CC BY SA.",
#'       side = 1, adj = 0, cex = 0.7, font = 3)
#' }
tilesLayer <- function(x, add = FALSE){
  if (add == FALSE) {
    plot.new()
    plot.window(xlim = c(x@extent@xmin, x@extent@xmax), 
                ylim = c(x@extent@ymin, x@extent@ymax), 
                xaxs = "i", yaxs = "i", asp = T)
  }
  
  raster::plotRGB(x = x, 
                  interpolate = T, 
                  maxpixels = raster::ncell(x), 
                  add  = TRUE)
}