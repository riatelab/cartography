#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a spatial object extent. Maps can be 
#' fetched from various open map servers.
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame.
#' @param spdf  deprecated, a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map, one of "osm", 
#' "opencycle", "hotstyle", "loviniahike", "loviniacycle", "hikebike", "osmgrayscale", 
#' "stamenbw", "stamenwatercolor", "osmtransport", "thunderforestlandscape", 
#' "thunderforestoutdoors", "cartodark", "cartolight".  
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @param crop TRUE if results should be cropped to the specified spdf extent, FALSE otherwise.
#' @details 
#' Zoom levels are descibed on the OpenStreetMap wiki: 
#' \url{http://wiki.openstreetmap.org/wiki/Zoom_levels}.
#' @note This function is a wrapper around the \code{osm.raster} function 
#' from the \code{rosm} package. \cr
#' Use directly the \href{https://CRAN.R-project.org/package=rosm}{rosm} package to have a finer control over  
#' extraction and display parameters.
#' @export
#' @import sp
#' @return A RatserBrick is returned.
#' @seealso \link{tilesLayer}
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
#' # Map tiles sources
#' mtext(text = "© OpenStreetMap contributors, under CC BY SA.",
#'       side = 1, adj = 0, cex = 0.7, font = 3)
#' 
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' # Download the tiles, extent = Martinique
#' mtqOSM <- getTiles(x = mtq, type = "osm", crop = TRUE)
#' # Plot the tiles
#' tilesLayer(mtqOSM)
#' # Plot countries
#' plot(st_geometry(mtq), add=TRUE)
#' mtext(text = "© OpenStreetMap contributors, under CC BY SA.",
#'       side = 1, adj = 0, cex = 0.7, font = 3)
#' }
getTiles <- function(x, spdf, type = "osm", zoom = NULL, crop = FALSE){
  
  if(!missing(spdf)){
    warning("spdf is deprecated; use x instead.", call. = FALSE)
  }
  
  if(!missing(x)){
    if(methods::is(x,"sf") == TRUE){
      spdf <- methods::as(x, "Spatial")
    }else{
      spdf <- x
    }
  }
  
  if (is.na(sp::proj4string(spdf))){
    stop("The Spatial object must contain information on its projection.",
         call. = FALSE)
  }
  
  if (!is.null(zoom)){zoom <- zoom + 1}
  
  finalOSM <- rosm::osm.raster(x = spdf, progress = "none", 
                               zoom = zoom, 
                               zoomin = -1,
                               cachedir = tempdir(), 
                               type = type, 
                               crop = crop)
  
  finalOSM@data@max <- rep(255,3)
  
  # detach(name = package:rosm)
  return(finalOSM)
}
