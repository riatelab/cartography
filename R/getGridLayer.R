#' @title Build a Regular Grid Layer
#' @name getGridLayer
#' @description Build a regular grid based on a SpatialPolygonsDataFrame. 
#' Provide also a table of surface intersections. 
#' @param spdf a SpatialPolygonsDataFrame.
#' @param cellsize output cell size, in map units.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param type shape of the cell, "regular" for squares, "hexagonal" for hexagons. 
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
getGridLayer <- function(spdf, cellsize, type = "regular", spdfid = NULL){
  # rgeos requirement
  # if (!requireNamespace("rgeos", quietly = TRUE)) {
  #   stop("'rgeos' package needed for this function to work. Please install it.",
  #        call. = FALSE)
  # }
  
  # id check
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  
  # spdf management/area
  spdf@data <- spdf@data[spdfid]
  row.names(spdf) <- spdf@data[,spdfid]
  spdf$area <- rgeos::gArea(spdf, byid=TRUE)

  # get a grid
  if(type %in% c("regular", "hexagonal")){
    spgrid <- switch(type, 
                     regular = getGridSquare(spdf, cellsize), 
                     hexagonal = getGridHexa(spdf, cellsize))
  }else{
    stop("type should be either 'regular' or 'hexagonal'", call. = F)
  }

  # keep only intersecting cells
  over <- rgeos::gIntersects(spgrid, spdf, byid = TRUE)
  x <- colSums(over)
  spgrid <- spgrid[spgrid$id %in% names(x[x>0]),]
  
  # intersection
  parts <- rgeos::gIntersection(spgrid, spdf, byid = TRUE)
  data <- data.frame(id=sapply(methods::slot(parts, "polygons"), 
                               methods::slot, "ID"))
  tmp <- data.frame(do.call('rbind', (strsplit(as.character(data$id)," "))))
  
  data$id1 <- as.vector(tmp$X1)
  data$id2 <- as.vector(tmp$X2)
  row.names(data) <- data$id
  parts <- SpatialPolygonsDataFrame(parts, data)
  proj4string(parts) <- proj4string(spdf)
  
  # 
  x <- rgeos::gBuffer(rgeos::gUnaryUnion(parts, id = parts$id1), byid=T)
  data <- data.frame(id = sapply(methods::slot(x, "polygons"),
                                 methods::slot, "ID"))
  row.names(data) <- data$id
  spgrid <- SpatialPolygonsDataFrame(x, data)
  spgrid@data$cell_area <- rgeos::gArea(spgrid, byid = TRUE)
  proj4string(spgrid) <-proj4string(spdf)
  
  #
  areas <- data.frame(parts@data, area_part = rgeos::gArea(parts, byid=TRUE), 
                      area_full = spdf@data[match(parts@data$id2, 
                                                  spdf@data[,spdfid]),"area"])
  areas$area_pct <- (areas$area_part/areas$area_full) * 100
  areas <- areas[,c("id1","id2","area_pct")]
  colnames(areas) <- c("id_cell","id_geo","area_pct")
  
  return(list(spdf = spgrid, df = areas))
}






getGridSquare <- function(spdf, cellsize){
  boundingBox <- bbox(spdf)
  rounder <- boundingBox %% cellsize
  boundingBox[,1] <- boundingBox[,1] - rounder[,1]
  roundermax <- cellsize - rounder[,2]
  boundingBox[,2] <- boundingBox[,2] + cellsize - rounder[,2]
  boxCoordX <- seq(from = boundingBox[1,1], 
                   to = boundingBox[1,2], 
                   by = cellsize)
  boxCoordY <- seq(from = boundingBox[2,1], 
                   to = boundingBox[2,2], 
                   by = cellsize)
  spatGrid <- expand.grid(boxCoordX, boxCoordY)
  spatGrid$id <- seq(1, nrow(spatGrid), 1)
  
  coordinates(spatGrid) <- 1:2 
  gridded(spatGrid) <- TRUE 
  spgrid <-  methods::as(spatGrid, "SpatialPolygonsDataFrame") 
  proj4string(spgrid) <-proj4string(spdf)
  row.names(spgrid) <- as.character(spgrid$id)
  return(spgrid)
}


getGridHexa <- function(spdf, cellsize){
  bbox <- bbox(spdf)
  bbox[, 1] <- bbox[, 1] - cellsize
  bbox[, 2] <- bbox[, 2] + cellsize
  bboxMat <- rbind( c(bbox[1,'min'] , bbox[2,'min']), 
                    c(bbox[1,'min'],bbox[2,'max']),
                    c(bbox[1,'max'],bbox[2,'max']), 
                    c(bbox[1,'max'],bbox[2,'min']), 
                    c(bbox[1,'min'],bbox[2,'min']) ) 
  bboxSP <- sp::SpatialPolygons(Srl = list(sp::Polygons(list(sp::Polygon(bboxMat)),"bbox")), 
                                proj4string=sp::CRS(sp::proj4string(spdf)))
  x <- sp::spsample(x = bboxSP, type = "hexagonal", 
                cellsize = cellsize, bb = bbox(spdf))
  grid <- sp::HexPoints2SpatialPolygons(x)
  grid <- sp::SpatialPolygonsDataFrame(Sr = grid, 
                                   data = data.frame(id = 1: length(grid)), 
                                   match.ID = FALSE)
  row.names(grid) <- as.character(grid$id)
  return(grid)
}


