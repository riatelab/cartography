#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a Spatial*DataFrame extent. Maps can be 
#' fetched from various open map servers.
#' @param spdf  a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map 
#' (see Details).
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @details 
#' Available tile servers are described in the openmap function of 
#' the OpenStreetMap package.
#' 
#' Zoom levels are descibed on the OpenStreetMap wiki: 
#' \url{http://wiki.openstreetmap.org/wiki/Zoom_levels}.
#' @note This function uses rgdal and OpenStreetMap packages. 
#' @export
#' @import sp
#' @return An OpenStreetMap object is returned.
#' @examples
#' \dontrun{
#' data("nuts2006")
#' # Download the tiles
#' StamWatCol <- getTiles(spdf = nuts0.spdf, type = "stamen-watercolor")
#' class(StamWatCol)
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
#' @examples
#' \dontrun{ 
#' data("nuts2006")
#' # Download the tiles
#' StamWatCol <- getTiles(spdf = nuts0.spdf, type = "stamen-watercolor")
#' #Display the tiles Layer
#' tilesLayer(x = StamWatCol)
#' #Display countries boundaries
#' plot(nuts0.spdf, add = TRUE)
#' }
tilesLayer <- function(x, add = FALSE){
  if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
    stop("'OpenStreetMap' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  OpenStreetMap::plot.OpenStreetMap(x, add = add, removeMargin = FALSE)
}





