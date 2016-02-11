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
#' data("nuts2006")
#' # extract Denmark
#' spdf <- nuts0.spdf[nuts0.spdf$id=="DK",]   
#' # Download the tiles, extent = Denmark 
#' den <- getTiles(spdf = spdf, type = "osm", crop = TRUE)
#' class(den)
#' # Plot the tiles
#' tilesLayer(den)
#' # Plot countries
#' plot(spdf, add=TRUE)
#' # Map tiles sources
#' mtext(text = "Map data © OpenStreetMap contributors, under CC BY SA.",
#'       side = 1, adj = 0, cex = 0.7, font = 3)
#' }
tilesLayer <- function(x, add = FALSE){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("'raster' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
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