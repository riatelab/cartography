#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a Spatial*DataFrame extent. Maps can be 
#' fetched from various open map servers.
#' @param spdf  a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map, one of "hikebike", 
#' "hotstyle", "lovinacycle", "lovinahike", "mapquestosm", "mapquestsat", "opencycle", 
#' "openpiste", "osm", "osmgrayscale", "osmtransport", "stamenbw", "stamenwatercolor",
#' "thunderforestlandscape" and "thunderforestoutdoors". 
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @param crop TRUE if results should be cropped to the specified spdf extent, FALSE otherwise.
#' @details 
#' Zoom levels are descibed on the OpenStreetMap wiki: 
#' \url{http://wiki.openstreetmap.org/wiki/Zoom_levels}.
#' @note This function is a wrapper around the \code{osrm.raster} function 
#' from the \code{rosm} package. \cr
#' Use directly the \href{http://cran.r-project.org/package=rosm}{rosm} package to have a finer control over  
#' extraction and display parameters.
#' @export
#' @import sp
#' @return A RatserBrick is returned.
#' @seealso \link{tilesLayer}
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
getTiles <- function(spdf, type = "osm", zoom = NULL, crop = FALSE){
  if (!requireNamespace("rosm", quietly = TRUE)) {
    stop("'rosm' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:rosm' %in% search()){
    
    attachNamespace('rosm')
  }
  if(!'package:raster' %in% search()){
    attachNamespace('raster')
  }
  
  if (is.na(sp::proj4string(spdf))){
    stop("The Spatial object must contain information on its projection.",
         call. = FALSE)
  }
  
  if (!is.null(zoom)){zoom <- zoom + 1}
  
  finalOSM <- rosm::osm.raster(x = spdf, 
                               zoom = zoom, 
                               zoomin = -1,
                               cachedir = tempdir(), 
                               type = type, 
                               crop = crop)
  
  finalOSM@data@max <- rep(255,3)
  
  detach(name = package:rosm)
  return(finalOSM)
}
