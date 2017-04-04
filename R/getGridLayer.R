#' @title Build a Regular Grid Layer
#' @name getGridLayer
#' @description Build a regular grid based on an sf object or a SpatialPolygonsDataFrame. 
#' Provide also a table of surface intersections. 
#' @param x an sf object, a simple feature collection (or a SpatialPolygonsDataFrame).
#' @param id  identifier field in x, default to the first column. (optional)
#' @param cellsize targeted area of the cell, in map units.
#' @param type shape of the cell, "regular" for squares, "hexagonal" for hexagons. 
#' @return A list is returned. The list contains "grid": a polygon sf object of 
#' a regular grid and "df":  a data frame of surface intersection. df fields are 
#' id_cell: ids of the grid; id_geo: ids of x objects and area_pct: share of the 
#' area of the polygon in the cell (a value of 55 means that 55\% of the sf unit area 
#' is within the cell).
#' @import sp
#' @import sf
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
getGridLayer <- function(x, id = NULL, cellsize, type = "regular"){
  # data(nuts2006)
  # nuts0.spdf@data  =  nuts0.df
  # x = nuts0.spdf
  # id = NULL
  # cellsize =  1000000 * 1000000
  # type = "regular"
  
  # sp check
  if (methods::is(x, 'Spatial')){
    x <- sf::st_as_sf(x)
  }
  
  # id check
  if (is.null(id)){id <- names(x)[1]}
  x <- x[, id]
  x$area <- sf::st_area(x)
  # row.names(x) <- as.character(x[[id]])
  
  
  # get a grid
  if(type %in% c("regular", "hexagonal")){
    grid <- switch(type, 
                   regular = getGridSquare(x, cellsize), 
                   hexagonal = getGridHexa(x, cellsize))
  }else{
    stop("type should be either 'regular' or 'hexagonal'", call. = F)
  }
  
  # keep only intersecting cells
  gover <- sf::st_intersects(grid, x)
  grid <- grid[unlist(lapply(gover, FUN = function(x) {if(length(x)>0){TRUE}else{FALSE}})), ]
  
  # predicted warning, we don't care...
  options(warn = -1)
  parts <- sf::st_intersection(x = grid[,"id_cell"], y = x[,id])
  options(warn = 0)

  # split parts
  l <- split(parts,  parts[[1]])
  # aggregate each parts
  a <- lapply(l, FUN = function(x){st_buffer(st_union(x), dist = 0.0000001)})
  # only polygons on   # bind all parts
  p <- st_cast(do.call(c, a))
  
  
  # full sf 
  grid <- st_sf(p, id = names(l))
  grid$gridarea <- st_area(x = grid)
  
  areas <- data.frame(parts[,1:2, drop = TRUE], area_part = sf::st_area(parts), 
                      area_full = x[match(parts[[2]], x[[id]]),]$area)
  
  areas$area_pct <- as.vector((areas$area_part/areas$area_full) * 100)
  areas <- areas[, c(1,2,5)]
  colnames(areas) <- c("id_cell","id_geo","area_pct")
  
  return(list(x = grid, df = areas))
}








getGridSquare <- function(x, cellsize){
  # cellsize transform
  cellsize = sqrt(cellsize)
  boundingBox <- st_bbox(x)
  rounder <- boundingBox[1:2] %% cellsize
  boundingBox[1] <- boundingBox[1] - rounder[1]
  boundingBox[2] <- boundingBox[2] - rounder[2]
  n <- unname(c(ceiling(diff(boundingBox[c(1, 3)]) / cellsize), 
                ceiling(diff(boundingBox[c(2, 4)]) / cellsize)))
  grid <- st_make_grid(cellsize = cellsize, offset = boundingBox[1:2], n = n,
                       crs = st_crs(x))
  grid <- st_sf(grid)
  grid$id_cell <- 1:nrow(grid)
  row.names(grid) <- grid$id_cell
  return(grid)
}


getGridHexa <- function(x, cellsize){
  
  #  •*´¨`*•.¸¸.•*´¨`*•.¸¸.•*´¨`*•.¸¸.•*´¨`*•.¸¸.•
  #
  #  This one is still in sp style !
  #
  #  •*´¨`*•.¸¸.•*´¨`*•.¸¸.•*´¨`*•.¸¸.•*´¨`*•.¸¸.•
  
  spdf <- methods::as(x, "Spatial")
  # cellsize transform
  cellsize <- 2 * sqrt(cellsize/((3*sqrt(3)/2))) * sqrt(3)/2
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
  
  pregrid <- sp::spsample(x = bboxSP, type = "hexagonal", cellsize = cellsize, 
                          bb = bbox(spdf))
  grid <- sp::HexPoints2SpatialPolygons(pregrid)
  grid <- sp::SpatialPolygonsDataFrame(Sr = grid, 
                                       data = data.frame(id_cell = 1:length(grid)), 
                                       match.ID = FALSE)
  row.names(grid) <- as.character(grid$id_cell)
  grid <- sf::st_as_sf(grid)
  return(grid)
}


