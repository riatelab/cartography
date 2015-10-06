#' @title Build a Regular Grid Layer
#' @name getGridLayer
#' @description Build a regular grid based on a SpatialPolygonsDataFrame. 
#' Provide also a table of surface intersections. 
#' @param spdf a SpatialPolygonsDataFrame.
#' @param cellsize output cell size, in map units.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @return A list is returned. The list contains "spdf": a SpatialPolygonsDataFrame of 
#' a regular grid and "df":  a data frame of surface intersection. df fields are id_cell: ids of the grid; 
#' id_geo: ids of the spdf and area_pct: share of the area of the polygon in the cell 
#' (a value of 55 means that 55\% of the spdf unit is within the cell).
#' @import sp
#' @seealso \link{getGridData}
#' @examples
#' \dontrun{
#' data(nuts2006)
#' # Get a grid layer
#' mygrid <- getGridLayer(spdf = nuts2.spdf, cellsize = 200000)
#' # Plot the grid
#' plot(mygrid$spdf)
#' head(mygrid$df)
#'        }
#' @export
getGridLayer <- function(spdf, cellsize, spdfid = NULL){
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("'rgeos' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  
  spdf@data <- spdf@data[spdfid]
  row.names(spdf@data)<-spdf@data[,spdfid]
  spdf <- spChFIDs(spdf, spdf@data[,spdfid])
  spdf@data$area <- rgeos::gArea(spdf, byid=TRUE)
  
  boundingBox <- bbox(spdf)
  rounder <- boundingBox %% cellsize
  boundingBox[,1] <- boundingBox[,1] - rounder[,1]
  roundermax <- cellsize - rounder[,2]
  boundingBox[,2] <- boundingBox[,2] + cellsize - rounder[,2]
  boxCoordX <- seq(from = boundingBox[1,1], to = boundingBox[1,2], 
                   by = cellsize)
  boxCoordY <- seq(from = boundingBox[2,1], to = boundingBox[2,2], 
                   by = cellsize)
  spatGrid <- expand.grid(boxCoordX, boxCoordY)
  spatGrid$id <- seq(1, nrow(spatGrid), 1)
  coordinates(spatGrid) <- 1:2 # promote to SpatialPointsDataFrame
  gridded(spatGrid) <- TRUE # promote to SpatialPixelsDataFrame
  spgrid <-  methods::as(spatGrid, "SpatialPolygonsDataFrame") # promote to SpatialPolygonDataFrame
  proj4string(spgrid) <-proj4string(spdf)
  row.names(spgrid) <- as.character(spgrid$id)
  
  # On ne garde que ce qui touche le fond de carte initial
  over <- rgeos::gIntersects(spgrid, spdf, byid = TRUE)
  x <- colSums(over)
  spgrid <- spgrid[spgrid$id %in% names(x[x>0]),]
  
  mask <- rgeos:: gBuffer(spdf, byid=FALSE, id=NULL, width=1.0, quadsegs=5, 
                          capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
  spgrid<- rgeos::gIntersection(spgrid, mask, byid=TRUE, 
                                id=as.character(spgrid@data$id), 
                                drop_lower_td=FALSE)
  data<-data.frame(id=sapply(methods::slot(spgrid, "polygons"), methods::slot, "ID"))
  row.names(data)<-data$id
  spgrid<-SpatialPolygonsDataFrame(spgrid, data)
  spgrid@data$cell_area <- rgeos::gArea(spgrid, byid=TRUE)
  proj4string(spgrid) <-proj4string(spdf)
  
  # On calcul la table de passage
  # intersection
  parts <- rgeos::gIntersection(spgrid, spdf, byid=TRUE,drop_lower_td=TRUE)
  data <- data.frame(id=sapply(methods::slot(parts, "polygons"), methods::slot, "ID"))
  tmp <- data.frame(do.call('rbind', (strsplit(as.character(data$id)," "))))
  data$id1 <- as.vector(tmp$X1)
  data$id2 <- as.vector(tmp$X2)
  row.names(data)<-data$id
  parts<-SpatialPolygonsDataFrame(parts, data)
  proj4string(parts) <-proj4string(spdf)
  
  # Part de surface intersectée
  parts@data$area_part <- rgeos::gArea(parts, byid=TRUE)
  parts@data <- data.frame(parts@data, 
                           area_full=spdf@data[match(
                             parts@data$id2, spdf@data[,spdfid]),"area"])
  parts@data$area_pct <- (parts@data$area_part/parts@data$area_full)*100
  areas <- parts@data[,c("id1","id2","area_pct")]
  colnames(areas) <- c("id_cell","id_geo","area_pct")
  return(list(spdf = spgrid, df = areas))
}