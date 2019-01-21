#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a spatial object extent. Maps can be 
#' fetched from various open map servers.
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame.
#' @param spdf  deprecated, a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map, one of "osm", "hotstyle", 
#' "hikebike", "osmgrayscale", "stamenbw", "stamenwatercolor", "cartodark", 
#' "cartolight".
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @param crop TRUE if results should be cropped to the specified x extent, FALSE otherwise.
#' @param verbose if TRUE a progress bar is displayed. 
#' @details 
#' Zoom levels are described on the OpenStreetMap wiki: 
#' \url{http://wiki.openstreetmap.org/wiki/Zoom_levels}.
#' @note This function is a wrapper around the \code{osm.raster} function 
#' from the \code{rosm} package. \cr
#' Use directly the \href{https://CRAN.R-project.org/package=rosm}{rosm} package to have a finer control over  
#' extraction and display parameters.
#' @export
#' @return A RatserBrick is returned.
#' @seealso \link{tilesLayer}
#' @examples
#' \dontrun{
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' # Download the tiles, extent = Martinique
#' mtqOSM <- getTiles(x = mtq, type = "osm", crop = TRUE)
#' # Plot the tiles
#' tilesLayer(mtqOSM)
#' # Plot countries
#' plot(st_geometry(mtq), add=TRUE)
#' txt <- "© OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright"
#' mtext(text = txt,
#'       side = 1, adj = 0, cex = 0.7, font = 3)
#' }
getTiles <- function(x, spdf, type = "osm", zoom = NULL, crop = FALSE, verbose = FALSE){
  
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
  
  if(verbose){
    progress <- "text"
  }else{
    progress <- "none"
  }
  
  finalOSM <- rosm::osm.raster(x = spdf, progress = progress, 
                               zoom = zoom, 
                               zoomin = -1,
                               cachedir = tempdir(), 
                               type = type, 
                               crop = crop)
  
  finalOSM@data@max <- rep(255,3)
  
  cat("Data and map tiles sources:\n")
  cit <- switch(
    type,
    "osm" = "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright.",
    "hotstyle" = "\u00A9 OpenStreetMap contributors. Tiles style by Humanitarian OpenStreetMap Team, under CC0, www.hotosm.org.",
    "hikebike" = "\u00A9 OpenStreetMap contributors. Tiles style under CC0, hikebikemap.net.", 
    "osmgrayscale" = "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright.",
    "stamenbw" = "\u00A9 OpenStreetMap contributors. Tiles style by Stamen Design, under CC BY 3.0, stamen.com.",
    "stamenwatercolor" = "\u00A9 OpenStreetMap contributors. Tiles style by Stamen Design, under CC BY 3.0, stamen.com.",
    "cartodark" = "\u00A9 OpenStreetMap contributors. Tiles style by Carto, under CC BY 3.0, carto.com/attribution.",
    "cartolight" = "\u00A9 OpenStreetMap contributors. Tiles style by Carto, under CC BY 3.0, carto.com/attribution."
  )
  cat(cit, "\n")
  
  return(finalOSM)
}

