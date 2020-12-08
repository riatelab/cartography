#' @title Get a regular grid
#' @name tc_get_grid
#' @description Build a regular grid based on an sf object of polygons.
#' @param x an sf object of polygons
#' @param var name of the numeric variable(s) in x to adapt to the grid (a vector)
#' @param cellsize targeted area of the cell, in map units
#' @param type shape of the cell, "regular" for squares, "hexagonal" for hexagons
#' @return A grid is returned as an sf object
#' @examples
#' mtq <- tc_import_mtq()
#' mtq_grid <- tc_get_grid(x = mtq, var = "POP", cellsize = 5000 * 5000)
#' mtq_grid$DENS <- mtq_grid$POP / mtq_grid$gridarea * 1000 * 1000
#' tc_map_c(mtq_grid, "DENS", pal = "Reds 2")
#' @export
tc_get_grid <- function(x, var, cellsize, type = "regular") {
  x$area <- st_area(x)
  # get a grid
  if (type %in% c("regular", "hexagonal")) {
    grid <- switch(type,
      regular = getGridAll(x, cellsize, TRUE),
      hexagonal = getGridAll(x, cellsize, FALSE)
    )
  } else {
    stop("type should be either 'regular' or 'hexagonal'", call. = FALSE)
  }
  # keep only intersecting cells
  gover <- st_intersects(grid, x)
  fun1 <- function(x) {
    if (length(x) > 0) {
      TRUE
    } else {
      FALSE
    }
  }
  grid <- grid[unlist(lapply(gover, FUN = fun1)), ]
  parts <- suppressWarnings(st_intersection(x = grid[, "id_cell"], y = x))
  parts$area_part <- st_area(parts)
  lvar <- vector(mode = "list", length = length(var))
  names(lvar) <- var
  for (i in 1:length(lvar)) {
    lvar[[i]] <- as.vector(parts[[names(lvar)[i]]] * parts$area_part /
      parts$area)
  }
  v <- aggregate(do.call(cbind, lvar),
    by = list(id = parts[["id_cell"]]),
    FUN = sum, na.rm = TRUE
  )
  # grid agg
  # split parts
  l <- split(parts, parts[[1]])
  # aggregate each parts
  a <- lapply(l, FUN = function(x) {
    st_buffer(st_union(x), dist = 0.0000001)
  })
  # only polygons on   # bind all parts
  geometry <- st_cast(do.call(c, a))
  # full sf
  grid <- st_sf(geometry, id = names(l))
  grid$gridarea <- st_area(x = grid)

  grid <- merge(grid, v, by = "id", all.x = TRUE)
  return(grid)
}

getGridAll <- function(x, cellsize, square) {
  # cellsize transform
  if (square) {
    cellsize <- sqrt(cellsize)
  } else {
    cellsize <- 2 * sqrt(cellsize / ((3 * sqrt(3) / 2))) * sqrt(3) / 2
  }
  boundingBox <- st_bbox(x)
  rounder <- boundingBox[1:2] %% cellsize
  boundingBox[1] <- boundingBox[1] - rounder[1]
  boundingBox[2] <- boundingBox[2] - rounder[2]
  n <- unname(c(
    ceiling(diff(boundingBox[c(1, 3)]) / cellsize),
    ceiling(diff(boundingBox[c(2, 4)]) / cellsize)
  ))
  grid <- st_make_grid(
    x = x, cellsize = cellsize, offset = boundingBox[1:2],
    n = n, crs = st_crs(x), square = square
  )
  grid <- st_sf(id_cell = 1:length(grid), geometry = grid)
  # grid$id_cell <- 1:nrow(grid)
  row.names(grid) <- grid$id_cell
  return(grid)
}