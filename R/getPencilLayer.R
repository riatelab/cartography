#' @title Pencil Layer
#' @name getPencilLayer
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
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' mtq_pencil <- getPencilLayer(x = mtq)
#' plot(st_geometry(mtq_pencil), col = 1:8)
#' plot(st_geometry(mtq), add = TRUE)
#' 
#' typoLayer(x = mtq_pencil, var="STATUT", add = FALSE,
#'           col = c("aquamarine4", "yellow3","wheat"),
#'           legend.values.order = c("Préfecture de région",
#'                                   "Sous-préfecture", 
#'                                   "Commune simple"),
#'           legend.pos = "topright",
#'           legend.title.txt = "Status")
#' plot(st_geometry(mtq), add = TRUE, ldy=2)
#' layoutLayer(title = "Commune Status",
#'             author = "UMS RIATE, 2017",
#'             sources = "IGN, 2016",
#'             scale = NULL)
#' @export
getPencilLayer <- function(x, size = 100, buffer = 1000, lefthanded = TRUE){
  a <- median(sf::st_area(sf::st_set_crs(x, NA)))
  size <- size * size
  . <- lapply(sf::st_geometry(x), makelines, size = size, buffer = buffer, 
              lefthanded = lefthanded, a = a)
  . <- sf::st_sfc(do.call(rbind,.))
  if(length(.)<nrow(x)){
    stop(simpleError("Try a smaller buffer size or a larger size"))
  }
  . <- sf::st_sf(geometry = ., x[,,drop=TRUE], sf_column_name = "geometry")
  . <- sf::st_set_crs(., sf::st_crs(x))
  . <- sf::st_cast(. , "MULTILINESTRING")
  return(.)
}

makelines <- function(x, size, buffer, lefthanded, a){
  size <- round(sqrt(as.numeric(sf::st_area(x) * size / a)), 0)
  if (size <= 10){size = 10}
  pt <- sf::st_sample(sf::st_buffer(x, buffer), size = size)
  # pt <- pt[sort(sample(1:length(pt), size, replace = FALSE))]
  if(lefthanded){
    pt <- sf::st_sf(pt, x = sf::st_coordinates(pt)[,2] + 
                      sf::st_coordinates(pt)[,1])
  } else{ 
    pt <- sf::st_sf(pt, x = sf::st_coordinates(pt)[,2] - 
                      sf::st_coordinates(pt)[,1])
  }
  pt <- sf::st_combine(pt[order(pt$x),])
  r <- sf::st_intersection(sf::st_cast(pt, "LINESTRING"), x)
}
