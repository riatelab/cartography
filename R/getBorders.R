#' @title Extract SpatialPolygonsDataFrame Borders
#' @description Extract borders between SpatialPolygonsDataFrame units.
#' @name getBorders
#' @param x an sf object, a simple feature collection (or a SpatialPolygonsDataFrame).
#' @param id identifier field in x, default to the first column. (optional)
#' @note getBorders and getOuterBorders can be combined with rbind. 
#' @return A SpatialLinesDataFrame of borders is returned. This object has three 
#' id fields: id, id1 and id2.
#' id1 and id2 are ids of units that neighbour a border; id is the concatenation 
#' of id1 and id2 (with "_" as separator).
#' @examples
#' data(nuts2006)
#' # Get units borders
#' nuts0.contig.spdf <- getBorders(st_as_sf(nuts0.spdf))
#' # Plot Countries
#' plot(nuts0.spdf, border = NA, col = "grey60")
#' # Plot borders
#' plot(st_geometry(nuts0.contig.spdf), 
#'      col = sample(x = rainbow(nrow(nuts0.contig.spdf))), 
#'      lwd = 3, add = TRUE)
#' @seealso \link{discLayer}, \link{getOuterBorders}
#' @import rgeos
#' @import sp
#' @import sf
#' @export
getBorders <- function(x, id = NULL){
  # library(sf)
  # data(nuts2006)
  # 
  # x = st_as_sf(nuts0.spdf)
  # id = NULL
  
  if(is.null(id)){id <- names(x)[1]}
  
  st_geometry(x) <-  st_buffer(st_geometry(x), 1)
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
  i<-1
  for (i in 1:length(myl)) {
    id1 <- names(myl[i])
    li <- x[x[[id]] == id1, ]
    j <- 1
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
  df <-  st_cast(x = df, to = "MULTILINESTRING")
  st_crs(df) <- st_crs(x)
  
  df2 <- df[, c(1,3,2)]
  
  names(df2) <- c("id", "id1", "id2", "geometry")
  df2$id <- paste(df2$id1, df2$id2, sep = "_")
  borderlines <- rbind(df, df2)
  row.names(borderlines) <- borderlines$id
  return(borderlines)
}
