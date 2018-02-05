#' @title Extract Polygons Borders
#' @description Extract borders between polygons.
#' @name getBorders
#' @param x an sf object, a simple feature collection or a SpatialPolygonsDataFrame.
#' @param id identifier field in x or spdf, default to the first column. (optional)
#' @param spdf deprecated, a SpatialPolygonsDataFrame. This SpatialPolygonsDataFrame
#'  has to be projected (planar coordinates). 
#' @param spdfid deprecated, identifier field in spdf, default to the first column of the 
#' spdf data frame. (optional)
#' @note getBorders and getOuterBorders can be combined with rbind. 
#' @return An sf object (MULTILINESTRING) of borders is returned. This object has three 
#' id fields: id, id1 and id2.
#' id1 and id2 are ids of units that neighbour a border; id is the concatenation 
#' of id1 and id2 (with "_" as separator).
#' @examples
#' library(sp)
#' library(sf)
#' data(nuts2006)
#' # Get borders
#' nuts0.contig <- getBorders(x = nuts0.spdf)
#' # Plot Countries
#' plot(nuts0.spdf, border = NA, col = "grey60")
#' # Plot borders
#' plot(st_geometry(nuts0.contig), 
#'      col = sample(x = rainbow(nrow(nuts0.contig))), 
#'      lwd = 3, add = TRUE)
#' 
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' # Get borders
#' mtq.borders <- getBorders(x = mtq)
#' # Plot polygons
#' plot(st_geometry(mtq), border = NA, col = "grey60")
#' # Plot borders
#' plot(st_geometry(mtq.borders), 
#'      col = sample(x = rainbow(nrow(nuts0.contig))), 
#'      lwd = 3, add = TRUE)
#' @seealso \link{discLayer}, \link{getOuterBorders}
#' @import rgeos
#' @import sp
#' @import sf
#' @export
getBorders <- function(x, id, spdf, spdfid = NULL){
  if(missing(x)){
    x <- sf::st_as_sf(spdf)
  }
  if(methods::is(x, 'Spatial')){
    x <- sf::st_as_sf(x)
  }
  if(!is.null(spdfid)){
    id <- spdfid
  }
  if(missing(id)){
    id <- names(x)[1]
  }
  
  if(sum(missing(spdf), is.null(spdfid)) != 2){
    warning("spdf and spdfid are deprecated; use x and id instead.", 
            call. = FALSE)
  }
  
  
  st_geometry(x) <-  st_buffer(x = st_geometry(x), 1, nQuadSegs = 5)
  lx <- st_cast(x, "MULTILINESTRING")
  
  l <- st_intersects(x,x, sparse = F)
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
      Inter <- st_intersection(st_geometry(li), st_geometry(po))
      df[ind,] <- c(paste0(id1,"_",id2), id1, id2)
      lgeo[[ind]]   <- Inter[[1]]
      ind <- ind + 1
    }
  }
  
  df <- st_sf(df, geometry = st_sfc(lgeo))
  df <- st_cast(x = df, to = "MULTILINESTRING")
  st_crs(df) <- st_crs(x)
  
  df2 <- df[, c(1,3,2)]
  
  names(df2) <- c("id", "id1", "id2", "geometry")
  df2$id <- paste(df2$id1, df2$id2, sep = "_")
  borderlines <- rbind(df, df2)
  row.names(borderlines) <- borderlines$id
  return(borderlines)
}








