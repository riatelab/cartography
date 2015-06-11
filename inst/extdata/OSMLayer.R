#### Class Definition
#' Class OSMLayer.
#'
#' Class OSMLayer defines an OSM layer .
#' @name OSMLayer-class
#' @exportClass OSMLayer
setClass (
  Class = "OSMLayer" ,
  slots = c(
    geom = "Spatial",
    type = "character",
    add="logical",
    zoom="numeric"
  )
  #   ,
  #   prototype=list(
  #     type = "osm",
  #     add = F,
  #     zoom = 12345678
  #   )
)

#### Methods declaration
#' Method AddOSMLayer
#' @name AddOSMLayer
#' @rdname AddOSMLayer-method
#' @param object Object of class OSMLayer
#' @exportMethod AddOSMLayer
#' @docType methods
setGeneric(
  name = "AddOSMLayer" ,
  def=function (object){ standardGeneric ("AddOSMLayer")
  }
)

#### Methods creation
#' @rdname AddOSMLayer-method
#' @import sp
#' @docType methods
setMethod("AddOSMLayer","OSMLayer",
          function (object){
            if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
              stop("'OpenStreetMap' package needed for this function to work. Please install it.",
                   call. = FALSE)
            }
            if (!requireNamespace("rgdal", quietly = TRUE)) {
              stop("'rgdal' package needed for this function to work. Please install it.",
                   call. = FALSE)
            }
            if (is.na(object@geom@proj4string)){
              stop("The Spatial object must contain information on its projection.",
                   call. = FALSE)
            }
            require(OpenStreetMap)
            require(rgdal)

            bboxGeom <- object@geom@bbox
            bboxGeomSP <- SpatialPoints(coords = data.frame(x = c(bboxGeom[1,1],bboxGeom[1,2] ),
                                                            y = c(bboxGeom[2,2],bboxGeom[2,1])),
                                        proj4string = object@geom@proj4string)
            latlon <- CRS("+proj=longlat +datum=WGS84")
            bboxGeomSP <- spTransform(x = bboxGeomSP, CRSobj = latlon)
            bboxGeomSP <- bboxGeomSP@bbox
            bboxGeomSP[2,2] <- bboxGeomSP[2,2] + 0.1 * (bboxGeomSP[2,2] - bboxGeomSP[2,1])
            bboxGeomSP[1,1] <- bboxGeomSP[1,1] - 0.1 * (bboxGeomSP[1,2] - bboxGeomSP[1,1])
            bboxGeomSP[2,1] <- bboxGeomSP[2,1] - 0.1 * (bboxGeomSP[2,2] - bboxGeomSP[2,1])
            bboxGeomSP[1,2] <- bboxGeomSP[1,2] + 0.1 * (bboxGeomSP[1,2] - bboxGeomSP[1,1])
            if(is.na(object@zoom)){
              tempOSM <- OpenStreetMap::openmap(upperLeft = c(bboxGeomSP[2,2],bboxGeomSP[1,1]),
                                                lowerRight = c(bboxGeomSP[2,1],bboxGeomSP[1,2]),
                                                type = object@type)
            } else {
              tempOSM <- OpenStreetMap::openmap(upperLeft = c(bboxGeomSP[2,2],bboxGeomSP[1,1]), zoom = object@zoom,
                                                lowerRight = c(bboxGeomSP[2,1],bboxGeomSP[1,2]),
                                                type = object@type)
            }
            finalOSM <- OpenStreetMap::openproj(x = tempOSM, projection = object@geom@proj4string)
            plot(finalOSM, add = object@add, removeMargin = F)
          }
)



#### Methods wrappers
#' OSMMap function.
#'
#' @name OSMMap
#' @param obj Spatial*DataFrame
#' @param type Type of map used in the \code{OpenStreetMap} package
#' @param zoom Zoom level of the map
#' @param add Whether to add the layer to an existing map (TRUE) or not (FALSE)
#' @export
#' @examples
#' data("TNdeleg")
#' # OSMMap(obj = TNdeleg.spdf, type = "osm", add = FALSE)
#' StaticMap(obj = TNdeleg.spdf, add = TRUE, col = NA)
#' LayoutMap(title = "Hell Yeah!", sources = "Sources Inconnues, OSM",
#'           author = "Mister T",scale = 150, frame = FALSE, north = TRUE )
OSMMap <- function(obj, type = "osm", zoom, add = FALSE){
  map <- new(Class = "OSMLayer")
  map@geom <- obj
  map@type <- type
  if(missing(zoom)){map@zoom <- NA_integer_} else {map@zoom <- zoom}
  map@add <- add
  AddOSMLayer(map)
}
