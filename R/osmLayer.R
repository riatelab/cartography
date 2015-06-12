#' @title Get Tiles From Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a Spatial*DataFrame extent. Maps can be 
#' fetched from various open map servers.
#' @param spdf Spatial*DataFrame; it must have a valid projection attribute.
#' @param type character; the tile server from which to get the map 
#' (see Details).
#' @param zoom numeric; the zoom level. If null, it is determined automatically 
#' (see Details).
#' @details This function uses rgdal and OpenStreetMap packages. 
#' 
#' Available tile servers are described in the \code{openmap} function of 
#' the OpenStreetMap package.
#' 
#' Zoom levels are descibed on the OpenStreetMap wiki: 
#' \url{http://wiki.openstreetmap.org/wiki/Zoom_levels}.
#' @export
#' @import sp
#' @return An OpenStreetMap object is returned.
#' @examples
#' data("nuts2006")
# 
# # Download the tiles
# StamWatCol <- getTiles(spdf = nuts0.spdf, type = "stamen-watercolor")
# class(StamWatCol)
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
    attachNamespace('raster')
    attachNamespace('OpenStreetMap')
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
#   detach(name = package:OpenStreetMap)
#   detach(name = package:raster)

  # plot(finalOSM, removeMargin = F)
  return(finalOSM)
}

#' @title Plot Tiles From Open Map Servers
#' @name tilesLayer
#' @param x OpenStreetMap object; \code{\link{getTiles}} outputs these objects.
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details This function is a wrapper for \code{plot.OpenStreetMap} from 
#' OpenStreetMap package.
#' @export
#' @examples
#' data("nuts2006")
# 
# # Download the tiles
# StamWatCol <- getTiles(spdf = nuts0.spdf, type = "stamen-watercolor")
# 
# # Display the tiles Layer
# tilesLayer(x = StamWatCol)
# 
# # Display countries boundaries
# plot(nuts0.spdf, add = TRUE)
# 
# # Layout plot
# layoutLayer(title = "Europe", 
#             sources = "Stamen, UMS RIATE, 2015", 
#             author = "UMS RIATE",
#             scale = NULL, 
#             frame = TRUE,
#             col = "black", 
#             coltitle = "white",
#             south = TRUE)
tilesLayer <- function(x, add = FALSE){
  if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
    stop("'OpenStreetMap' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  OpenStreetMap::plot.OpenStreetMap(x, add = add, removeMargin = FALSE)
}





