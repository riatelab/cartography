#' @title Extract Polygons Borders
#' @description Extract borders between polygons.
#' @name getBorders
#' @param x an sf object, a simple feature collection or a SpatialPolygonsDataFrame.
#' @param id name of the identifier variable x, default to the first column. (optional)
#' @param spdf defunct. 
#' @param spdfid defunct. 
#' @note getBorders and getOuterBorders can be combined with rbind. 
#' @return An sf object (MULTILINESTRING) of borders is returned. This object has three 
#' id variables: id, id1 and id2.
#' id1 and id2 are ids of units that neighbour a border; id is the concatenation 
#' of id1 and id2 (with "_" as separator).
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' # Get borders
#' mtq.borders <- getBorders(x = mtq)
#' # Plot polygons
#' plot(st_geometry(mtq), border = NA, col = "grey60")
#' # Plot borders
#' plot(st_geometry(mtq.borders), 
#'      col = sample(x = rainbow(nrow(mtq.borders))), 
#'      lwd = 3, add = TRUE)
#' @seealso \link{discLayer}
#' @export
getBorders <- function(x, id, spdf, spdfid){

  
  if(methods::is(x, 'Spatial')){
    x <- sf::st_as_sf(x)
  }

  if(missing(id)){
    id <- names(x)[1]
  }
  
  if(sum(missing(spdf), missing(spdfid)) != 2){
    stop("spdf and spdfid are defunct; use x and id instead.", 
            call. = FALSE)
  }
  
  
  sf::st_geometry(x) <-  sf::st_buffer(x = sf::st_geometry(x), 1, nQuadSegs = 5)
  lx <- sf::st_cast(x, "MULTILINESTRING")
  
  l <- sf::st_intersects(x,x, sparse = FALSE)
  colnames(l) <- x[[id]]
  rownames(l) <- x[[id]]
  l <- lower.tri(l) * l
  
  gna <- function(x) {
    y <- x[x == 1]
    if (length(y) > 0) {
      names(y)
    } else {
      NA
    }
  }
  myl <- as.list(apply(l, 1, gna))
  myl <- myl[!is.na(myl)]
  long <- sum(sapply(myl, length))
  df <- data.frame(id = rep(NA, long), id1 = rep(NA, long), id2 = rep(NA, long))
  
  lgeo <- vector(mode = "list", length = long)
  lgeo2 <- vector(mode = "list", length = long)
  ind <- 1
  for (i in 1:length(myl)) {
    id1 <- names(myl[i])
    li <- lx[lx[[id]] == id1, ]
    for (j in 1:length(myl[[i]])) {
      id2 <- myl[[i]][[j]]
      po <- x[x[[id]] == id2, ]
      Inter <- sf::st_intersection(sf::st_geometry(li), sf::st_geometry(po))
      df[ind,] <- c(paste0(id1,"_",id2), id1, id2)
      lgeo[[ind]]   <- Inter[[1]]
      ind <- ind + 1
    }
  }
  
  df <- sf::st_sf(df, geometry = sf::st_sfc(lgeo))
  df <- sf::st_cast(x = df, to = "MULTILINESTRING")
  sf::st_set_crs(df, sf::st_crs(x))
  
  df2 <- df[, c(1,3,2)]
  
  names(df2) <- c("id", "id1", "id2", "geometry")
  df2$id <- paste(df2$id1, df2$id2, sep = "_")
  borderlines <- rbind(df, df2)
  row.names(borderlines) <- borderlines$id
  return(borderlines)
}








