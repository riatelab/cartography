#' @title Plot Tiles from Open Map Servers
#' @description Plot tiles from open map servers.
#' @name tilesLayer
#' @param x an OpenStreetMap object; the \link{getTiles} function outputs these 
#' objects.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @note This function is a wrapper for plot.OpenStreetMap from the 
#' OpenStreetMap package.
#' @export
#' @seealso \link{getTiles}
#' @examples
#' \dontrun{
#' data("nuts2006")
#' # Download the tiles, nuts0.spdf extent
#' StamWatCol <- getTiles(spdf = nuts0.spdf, type = "stamen-watercolor")
#' class(StamWatCol)
#' # Plot the tiles
#' tilesLayer(StamWatCol)
#' # Plot countries
#' plot(nuts0.spdf, add=TRUE)
#' }
tilesLayer <- function(x, add = FALSE){
  if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
    stop("'OpenStreetMap' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  OpenStreetMap::plot.OpenStreetMap(x, add = add, removeMargin = FALSE)
}





