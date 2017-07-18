#' @title Build a Regular Grid Layer
#' @name getGridLayer
#' @description Build a regular grid based on an sf object or a SpatialPolygonsDataFrame. 
#' Provide also a table of surface intersections. 
#' @param x an sf object, a simple feature collection (or a SpatialPolygonsDataFrame).
#' @param cellsize targeted area of the cell, in map units.
#' @param type shape of the cell, "regular" for squares, "hexagonal" for hexagons. 
#' @param var name of the numeric field(s) in x to adapt to the grid (a vector).
#' @return A grid is returned as an sf object.
#' @import sp
#' @import sf
#' @examples
#' \dontrun{
#'   library(sf)
#'   data(nuts2006)
#'   nuts2.spdf@data = nuts2.df
#'   mygrid <- getGridLayer(x = sf::st_as_sf(nuts2.spdf), cellsize = 200000 * 200000, 
#'                          type = "regular", var = "pop2008")
#'   # Plot total population
#'   plot(st_geometry(mygrid), col="#CCCCCC",border="white")
#'   propSymbolsLayer(x = mygrid, var = "pop2008", 
#'                    legend.style = "e", legend.pos = "right", border = "white",
#'                    legend.title.txt = "Total population",
#'                    inches=0.1, col="black", add=TRUE)
#'   
#'   # Plot dentsity of population 
#'   ## conversion from square meter to square kilometers
#'   mygrid$densitykm <- mygrid$pop2008 * 1000 * 1000 / mygrid$gridarea 
#'   cols <- carto.pal(pal1 = "taupe.pal", n1 = 6)
#'   choroLayer(x = mygrid, var = "densitykm", 
#'              border = "grey80",col=cols, add=FALSE,
#'              legend.pos = "right", method = "q6", legend.values.rnd = 1,
#'              legend.title.txt = "Population density")
#' }
#' @export
getGridLayer <- function(x, cellsize, type = "regular", var){
  # sp check
  if (methods::is(x, 'Spatial')){
    x <- sf::st_as_sf(x)
  }

  x$area <- sf::st_area(x)
  
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
  parts <- sf::st_intersection(x = grid[,"id_cell"], y = x)
  options(warn = 0)
  parts$area_part <- sf::st_area(parts)

  lvar <- vector(mode = "list", length = length(var))
  names(lvar) <- var
  for (i in 1:length(lvar)){
    lvar[[i]] <- as.vector(parts[[names(lvar)[i]]] * parts$area_part / parts$area)
  }
  v <- aggregate(do.call(cbind,lvar), by = list(id = parts[['id_cell']]), 
                 FUN = sum, na.rm=TRUE)
  
  # grid agg
  # split parts
  l <- split(parts,  parts[[1]])
  # aggregate each parts
  a <- lapply(l, FUN = function(x){st_buffer(st_union(x), dist = 0.0000001)})
  # only polygons on   # bind all parts
  geometry <- st_cast(do.call(c, a))
  # full sf 
  grid <- st_sf(geometry, id = names(l))
  grid$gridarea <- st_area(x = grid)
  
  grid <- merge(grid, v, by = "id", all.x = T)
  return(grid)

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
  grid <- st_sf(id_cell=1:length(grid), geometry = grid)
  # grid$id_cell <- 1:nrow(grid)
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


# getGridLayer2 <- function(x, cellsize, type = "regular", var){
#   # sp check
#   if (methods::is(x, 'Spatial')){
#     x <- sf::st_as_sf(x)
#   }
#   # get a grid
#   if(type %in% c("regular", "hexagonal")){
#     grid <- switch(type, 
#                    regular = getGridSquare(x, cellsize), 
#                    hexagonal = getGridHexa(x, cellsize))
#   }else{
#     stop("type should be either 'regular' or 'hexagonal'", call. = F)
#   }
#   # predicted warning, we don't care...
#   options(warn = -1)
#   x <- sf::st_buffer(x=x,dist=0.0000001, nQuadSegs = 5)
#   xx <- st_interpolate_aw(x = x[var], to = grid, extensive = T)
#   parts <- sf::st_intersection(x = xx, y = st_union(x))
#   options(warn = 0)
#   grid <- st_cast(parts, to = "MULTIPOLYGON")
#   names(grid)[1] <- "id"
#   return(grid)
# }
