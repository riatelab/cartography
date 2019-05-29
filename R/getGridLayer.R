#' @title Build a Regular Grid Layer
#' @name getGridLayer
#' @description Build a regular grid based on an sf object or a SpatialPolygonsDataFrame. 
#' @param x an sf object, a simple feature collection or a SpatialPolygonsDataFrame.
#' @param cellsize targeted area of the cell, in map units.
#' @param type shape of the cell, "regular" for squares, "hexagonal" for hexagons. 
#' @param var name of the numeric variable(s) in x to adapt to the grid (a vector).
#' @param spdf deprecated, a SpatialPolygonsDataFrame.
#' @param spdfid deprecated, identifier field in spdf, default to the first column 
#' of the spdf data frame.  (optional)
#' @return A grid is returned as an sf object.
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' # Plot dentsity of population
#' mtq$POPDENS <- 1e6 * mtq$POP / st_area(mtq) 
#' bks <- getBreaks(v = mtq$POPDENS, method = "geom", 5)
#' cols <- carto.pal(pal1 = "taupe.pal", n1 = 5)
#' opar <- par(mfrow = c(1,2), mar = c(0,0,0,0))
#' choroLayer(x = mtq, var = "POPDENS", breaks = bks,
#'            border = "burlywood3", col = cols,
#'            legend.pos = "topright", legend.values.rnd = 0,
#'            legend.title.txt = "Population density")
#' 
#' mygrid <- getGridLayer(x = mtq, cellsize = 3e7,
#'                        type = "hexagonal", var = "POP")
#' ## conversion from square meter to square kilometers
#' mygrid$POPDENSG <- 1e6 * mygrid$POP / mygrid$gridarea 
#' choroLayer(x = mygrid, var = "POPDENSG", breaks = bks,
#'            border = "burlywood3", col = cols,
#'            legend.pos = "n", legend.values.rnd = 1,
#'            legend.title.txt = "Population density")
#' par(opar)
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
                   regular = getGridAll(x, cellsize, TRUE),
                   hexagonal = getGridAll(x, cellsize, FALSE))
  }else{
    stop("type should be either 'regular' or 'hexagonal'", call. = FALSE)
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
  
  grid <- merge(grid, v, by = "id", all.x = TRUE)
  return(grid)
}

getGridAll <- function(x, cellsize, square){
  # cellsize transform
  if (square){
    cellsize <- sqrt(cellsize)
  }else{
    cellsize <- 2 * sqrt(cellsize/((3*sqrt(3)/2))) * sqrt(3)/2
  }
  boundingBox <- sf::st_bbox(x)
  rounder <- boundingBox[1:2] %% cellsize
  boundingBox[1] <- boundingBox[1] - rounder[1]
  boundingBox[2] <- boundingBox[2] - rounder[2]
  n <- unname(c(ceiling(diff(boundingBox[c(1, 3)]) / cellsize),
                ceiling(diff(boundingBox[c(2, 4)]) / cellsize)))
  grid <- sf::st_make_grid(x = x, cellsize = cellsize, offset = boundingBox[1:2],
                           n = n, crs = sf::st_crs(x), square = square)
  grid <- sf::st_sf(id_cell=1:length(grid), geometry = grid)
  # grid$id_cell <- 1:nrow(grid)
  row.names(grid) <- grid$id_cell
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