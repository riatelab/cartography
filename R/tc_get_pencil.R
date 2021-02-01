#' @title Get a pencil layer
#' @name tc_get_pencil
#' @description Create a pencil layer. This function transforms a POLYGON or
#' MULTIPOLYGON sf object into a MULTILINESTRING one.
#' @param x an sf object, a simple feature collection (POLYGON or MULTIPOLYGON).
#' @param size density of the penciling. Median number of points used to build
#' the MULTILINESTRING.
#' @param buffer buffer around each polygon. This buffer (in map units) is used
#' to take sample points. A negative value adds a margin between the penciling
#' and the original polygons borders
#' @param lefthanded if TRUE the penciling is done left-handed style.
#' @return A MULTILINESTRING sf object is returned.
#' @examples
#' mtq <- tc_import_mtq()
#' mtq_pencil <- tc_get_pencil(x = mtq, size = 100, buffer = 1000,
#'                             lefthanded = TRUE)
#' tc_map(mtq_pencil)
#' @export
tc_get_pencil <- function(x, size = 100, buffer = 1000, lefthanded = TRUE) {
  a <- median(st_area(st_set_crs(x, NA)))
  size <- size * size
  . <- lapply(st_geometry(x), makelines,
    size = size, buffer = buffer,
    lefthanded = lefthanded, a = a
  )
  . <- st_sfc(do.call(rbind, .))
  if (length(.) < nrow(x)) {
    stop(simpleError("Try a smaller buffer size or a larger size"))
  }
  . <- st_sf(geometry = ., st_drop_geometry(x), sf_column_name = "geometry")
  . <- st_set_crs(., st_crs(x))
  . <- st_cast(., "MULTILINESTRING")
  return(.)
}

makelines <- function(x, size, buffer, lefthanded, a) {
  size <- round(sqrt(as.numeric(st_area(x) * size / a)), 0)
  if (size <= 10) {
    size <- 10
  }
  pt <- st_sample(st_buffer(st_sfc(x), buffer), size = size)
  # pt <- pt[sort(sample(1:length(pt), size, replace = FALSE))]
  if (lefthanded) {
    pt <- st_sf(pt, x = st_coordinates(pt)[, 2] +
      st_coordinates(pt)[, 1])
  } else {
    pt <- st_sf(pt, x = st_coordinates(pt)[, 2] -
      st_coordinates(pt)[, 1])
  }
  pt <- st_combine(pt[order(pt$x), ])
  r <- st_intersection(st_cast(pt, "LINESTRING"), x)
}