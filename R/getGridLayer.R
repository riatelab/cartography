#' @title Build a Regular Grid Layer
#' @name getGridLayer
#' @description Build a regular grid based on an sf object or a SpatialPolygonsDataFrame. 
#' @param x an sf object, a simple feature collection or a SpatialPolygonsDataFrame.
#' @param cellsize targeted area of the cell, in map units.
#' @param type shape of the cell, "regular" for squares, "hexagonal" for hexagons. 
#' @param var name of the numeric field(s) in x to adapt to the grid (a vector).
#' @param spdf deprecated, a SpatialPolygonsDataFrame.
#' @param spdfid deprecated, identifier field in spdf, default to the first column 
#' of the spdf data frame.  (optional)
#' @return A grid is returned as an sf object.
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' # Plot dentsity of population 
#' mtq$dens <- mtq$P13_POP / (st_area(mtq) / (1000 * 1000)) 
#' bks <- getBreaks(v = mtq$dens, method = "q6")
#' cols <- carto.pal(pal1 = "taupe.pal", n1 = 6)
#' opar <- par(mfrow = c(1,2), mar = c(0,0,0,0))
#' choroLayer(x = mtq, var = "dens", breaks = bks, 
#'            border = "burlywood3", col = cols, 
#'            legend.pos = "topright", legend.values.rnd = 1,
#'            legend.title.txt = "Population density")
#' 
#' mygrid <- getGridLayer(x = mtq, cellsize = 3000 * 3000, 
#'                        type = "regular", var = "P13_POP")
#' ## conversion from square meter to square kilometers
#' mygrid$densitykm <- mygrid$P13_POP / (mygrid$gridarea / (1000 * 1000)) 
#' choroLayer(x = mygrid, var = "densitykm", breaks = bks,
#'            border = "burlywood3", col = cols, 
#'            legend.pos = "n", legend.values.rnd = 1,
#'            legend.title.txt = "Population density")
#' plot(st_geometry(mtq), lwd = 0.2, add=TRUE, border = "#ffffff75")
#' 
#' \donttest{
#' library(sp)
#' data(nuts2006)
#' nuts2.spdf@data = nuts2.df
#' mygrid <- getGridLayer(x = nuts2.spdf, cellsize = 200000 * 200000, 
#'                        type = "regular", var = "pop2008")
#' # Plot total population
#' plot(st_geometry(mygrid), col="#CCCCCC",border="white")
#' propSymbolsLayer(x = mygrid, var = "pop2008", border = "white",
#'                  legend.style = "e", legend.pos = "right", 
#'                  legend.title.txt = "Total population",
#'                  inches = 0.1, col = "black", add = TRUE)
#' 
#' # Plot dentsity of population 
#' ## conversion from square meter to square kilometers
#' mygrid$densitykm <- mygrid$pop2008 * 1000 * 1000 / mygrid$gridarea 
#' cols <- carto.pal(pal1 = "taupe.pal", n1 = 6)
#' choroLayer(x = mygrid, var = "densitykm", 
#'            border = "grey80",col = cols, method = "q6", 
#'            legend.pos = "right", legend.values.rnd = 1,
#'            legend.title.txt = "Population density")
#' par(opar)
#' }
#' @export
getGridLayer <- function(x, cellsize, type = "regular", var, 
                         spdf, spdfid = NULL){
  # sp check
  if(missing(x)){
    x <- sf::st_as_sf(spdf)
  }
  if (methods::is(x, 'Spatial')){
    x <- sf::st_as_sf(x)
  }
  
  if(sum(missing(spdf), is.null(spdfid)) != 2){
    warning("spdf and spdfid are deprecated; use x instead.", call. = FALSE)
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
  fun1 <- function(x) {if(length(x)>0){TRUE}else{FALSE}} 
  grid <- grid[unlist(lapply(gover, FUN = fun1)), ]
  
  # predicted warning, we don't care...
  options(warn = -1)
  parts <- sf::st_intersection(x = grid[,"id_cell"], y = x)
  options(warn = 0)
  parts$area_part <- sf::st_area(parts)

  lvar <- vector(mode = "list", length = length(var))
  names(lvar) <- var
  for (i in 1:length(lvar)){
    lvar[[i]] <- as.vector(parts[[names(lvar)[i]]] * parts$area_part / 
                             parts$area)
  }
  v <- aggregate(do.call(cbind,lvar), by = list(id = parts[['id_cell']]), 
                 FUN = sum, na.rm=TRUE)
  
  # grid agg
  # split parts
  l <- split(parts,  parts[[1]])
  # aggregate each parts
  a <- lapply(l, FUN = function(x){sf::st_buffer(sf::st_union(x), dist = 0.0000001)})
  # only polygons on   # bind all parts
  geometry <- sf::st_cast(do.call(c, a))
  # full sf 
  grid <- sf::st_sf(geometry, id = names(l))
  grid$gridarea <- sf::st_area(x = grid)
  
  grid <- merge(grid, v, by = "id", all.x = T)
  return(grid)

}


getGridSquare <- function(x, cellsize){
  # cellsize transform
  cellsize <- sqrt(cellsize)
  boundingBox <- sf::st_bbox(x)
  rounder <- boundingBox[1:2] %% cellsize
  boundingBox[1] <- boundingBox[1] - rounder[1]
  boundingBox[2] <- boundingBox[2] - rounder[2]
  n <- unname(c(ceiling(diff(boundingBox[c(1, 3)]) / cellsize), 
                ceiling(diff(boundingBox[c(2, 4)]) / cellsize)))
  grid <- sf::st_make_grid(cellsize = cellsize, offset = boundingBox[1:2], n = n,
                       crs = sf::st_crs(x))
  grid <- sf::st_sf(id_cell=1:length(grid), geometry = grid)
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
  bbox <- sp::bbox(spdf)
  bbox[, 1] <- bbox[, 1] - cellsize
  bbox[, 2] <- bbox[, 2] + cellsize
  bboxMat <- rbind( c(bbox[1,'min'] , bbox[2,'min']), 
                    c(bbox[1,'min'],bbox[2,'max']),
                    c(bbox[1,'max'],bbox[2,'max']), 
                    c(bbox[1,'max'],bbox[2,'min']), 
                    c(bbox[1,'min'],bbox[2,'min']) ) 
  bboxSP <- sp::SpatialPolygons(Srl = 
                                  list(sp::Polygons(list(sp::Polygon(bboxMat)),
                                                    "bbox")), 
                                proj4string=sp::CRS(sp::proj4string(spdf)))
  
  pregrid <- sp::spsample(x = bboxSP, type = "hexagonal", cellsize = cellsize, 
                          bb = bbox(spdf))
  grid <- sp::HexPoints2SpatialPolygons(pregrid)
  grid <- sp::SpatialPolygonsDataFrame(Sr = grid, 
                                       data = data.frame(id_cell = 
                                                           1:length(grid)), 
                                       match.ID = FALSE)
  row.names(grid) <- as.character(grid$id_cell)
  grid <- sf::st_as_sf(grid)
  return(grid)
}


#' @title Compute Data for a Grid Layer
#' @name getGridData
#' @description Defunct
#' @param x ...
#' @param df ...
#' @param dfid ...
#' @param var ...
#' @export
getGridData <- function(x, df, dfid = NULL, var){
  .Defunct(msg = "This function is defunct, use getGridLayer instead.")
}

