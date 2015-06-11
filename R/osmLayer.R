#' @title Get an OpenStreetMap Object
#' @param spdf Spatial*DataFrame with projection information
#' @param type Type of map used in the \code{OpenStreetMap} package
#' @param zoom Zoom level of the map
#' @details This function uses rgdal and OpenStreetMap packages.
#' @export
#' @import sp
#' @examples
#' data("TNdeleg")
getOSMLayer <- function(spdf, type = "osm", zoom = NULL){
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
  detach(name = package:raster)
  detach(name = package:OpenStreetMap)
  # plot(finalOSM, removeMargin = F)
  return(finalOSM)
}

#' @title Plot an OpenStreetMap Layer
#' @param x OpenStreetMap object
#' @param add Whether to add the layer to an existing map (TRUE) or not (FALSE)
#' @details This function is a wrapper for plot.OpenStreetMap
#' @export
#' @examples
#' data("TNdeleg")
osmLayer <- function(x, add = FALSE){
  if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
    stop("'OpenStreetMap' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  OpenStreetMap::plot.OpenStreetMap(x, add = add, removeMargin = FALSE)
}



