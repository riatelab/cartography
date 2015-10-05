#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a Spatial*DataFrame extent. Maps can be 
#' fetched from various open map servers.
#' @param spdf  a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map, one of "osm", "osm-bw", 
#' "maptoolkit-topo", "waze", "mapquest", "mapquest-aerial", "bing", 
#' "stamen-toner", "stamen-terrain", "stamen-watercolor", "osm-german", 
#' "osm-wanderreitkarte", "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", 
#' "skobbler", "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", 
#' "osm-public-transport", "osm-bbike", "osm-bbike-german".
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @details 
#' Zoom levels are descibed on the OpenStreetMap wiki: 
#' \url{http://wiki.openstreetmap.org/wiki/Zoom_levels}.
#' @note This function uses \code{rgdal} and \code{OpenStreetMap} packages. 
#' @export
#' @import sp
#' @return An OpenStreetMap object is returned.
#' @seealso \link{tilesLayer}
#' @examples
#' \dontrun{
#' data("nuts2006")
#' # Download the tiles, nuts0.spdf extent
#' EuropeOsm <- getTiles(spdf = nuts0.spdf, type = "osm")
#' class(EuropeOsm)
#' # Plot the tiles
#' tilesLayer(EuropeOsm)
#' # Plot countries
#' plot(nuts0.spdf, add=TRUE)
#' # Map tiles sources
#' mtext(text = "Map data © OpenStreetMap contributors, under CC BY SA.", 
#'       side = 1, adj = 0, cex = 0.7, font = 3)
#' }
getTiles <- function(spdf, type = "osm", zoom = NULL){
  if (!requireNamespace("rgdal", quietly = TRUE)) {
    stop("'rgdal' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
    stop("'OpenStreetMap' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:OpenStreetMap' %in% search()){
    
    attachNamespace('OpenStreetMap')
  }
  if(!'package:raster' %in% search()){
    attachNamespace('raster')
  }
  
  
  if (is.na(sp::proj4string(spdf))){
    stop("The Spatial object must contain information on its projection.",
         call. = FALSE)
  }
  bboxGeom <- sp::bbox(spdf)
  bboxGeomSP <- sp::SpatialPoints(coords = data.frame(x = c(bboxGeom[1,1],bboxGeom[1,2] ),
                                                      y = c(bboxGeom[2,2],bboxGeom[2,1])),
                                  proj4string = sp::CRS(sp::proj4string(spdf)))
  latlon <- sp::CRS("+proj=longlat +datum=WGS84")
  bboxGeomSP <- sp::spTransform(x = bboxGeomSP, CRSobj = latlon)
  bboxGeomSP <- sp::bbox(bboxGeomSP)
  bboxGeomSP[2,2] <- bboxGeomSP[2,2] + 0.1 * (bboxGeomSP[2,2] - bboxGeomSP[2,1])
  bboxGeomSP[1,1] <- bboxGeomSP[1,1] - 0.1 * (bboxGeomSP[1,2] - bboxGeomSP[1,1])
  bboxGeomSP[2,1] <- bboxGeomSP[2,1] - 0.1 * (bboxGeomSP[2,2] - bboxGeomSP[2,1])
  bboxGeomSP[1,2] <- bboxGeomSP[1,2] + 0.1 * (bboxGeomSP[1,2] - bboxGeomSP[1,1])
  if(is.null(zoom)){
    tempOSM <- OpenStreetMap::openmap(upperLeft = c(bboxGeomSP[2,2],bboxGeomSP[1,1]),
                                      lowerRight = c(bboxGeomSP[2,1],bboxGeomSP[1,2]),
                                      type = type)
  } else {
    tempOSM <- OpenStreetMap::openmap(upperLeft = c(bboxGeomSP[2,2],bboxGeomSP[1,1]), zoom = zoom,
                                      lowerRight = c(bboxGeomSP[2,1],bboxGeomSP[1,2]),
                                      type = type)
  }
  finalOSM <- OpenStreetMap::openproj(x = tempOSM, projection = sp::CRS(sp::proj4string(spdf)))
  detach(name = package:OpenStreetMap)
  #   detach(name = package:raster)
  
  # plot(finalOSM, removeMargin = F)
  return(finalOSM)
}