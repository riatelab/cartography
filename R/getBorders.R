#' @title Extract SpatialPolygonsDataFrame Borders
#' @description Extract borders between SpatialPolygonsDataFrame units.
#' @name getBorders
#' @param x an sf object, a simple feature collection.
#' @param spdf a SpatialPolygonsDataFrame. This SpatialPolygonsDataFrame
#'  has to be projected (planar coordinates).
#' @param id identifier field in x or spdf, default to the first column. (optional)
#' @note getBorders and getOuterBorders can be combined with rbind. 
#' @return An sf object (MULTILINESTRING) of borders is returned. This object has three 
#' id fields: id, id1 and id2.
#' id1 and id2 are ids of units that neighbour a border; id is the concatenation 
#' of id1 and id2 (with "_" as separator).
#' @examples
#' data(nuts2006)
#' # Get units borders
#' nuts0.contig <- getBorders(x = st_as_sf(nuts0.spdf))
#' # Plot Countries
#' plot(nuts0.spdf, border = NA, col = "grey60")
#' # Plot borders
#' plot(st_geometry(nuts0.contig), 
#'      col = sample(x = rainbow(nrow(nuts0.contig))), 
#'      lwd = 3, add = TRUE)
#' @seealso \link{discLayer}, \link{getOuterBorders}
#' @import rgeos
#' @import sp
#' @import sf
#' @export
getBorders <- function(x, spdf, id = NULL){
  if(!missing(x)){spdf <- methods::as(x, "Spatial")}
  
  if (is.null(id)) {
    id <- names(spdf@data)[1]
  }
  spdf@data <- spdf@data[id]
  colnames(spdf@data)[1] <- "id"
  row.names(spdf) <- as.character(spdf@data$id)
  
  spdf <- gBuffer(spdf, width = 1, byid = T)
  
  df <- data.frame(id = sapply(methods::slot(spdf, "polygons"),
                               methods::slot, "ID"))
  row.names(df) <- df$id
  spdf <- SpatialPolygonsDataFrame(Sr = spdf, data = df)
  sldf <- methods::as(object = spdf, Class = "SpatialLines")
  df <- data.frame(id = sapply(methods::slot(sldf, "lines"),
                               methods::slot, "ID"))
  row.names(df) <- df$id
  sldf <- SpatialLinesDataFrame(sl = sldf, data = df)
  l <-gIntersects(spdf,
                  checkValidity = TRUE,
                  byid = T,
                  returnDense = T)
  l <- lower.tri(l) * l
  gna <- function(x) {
    y <- x[x == 1]
    if (length(y) > 0) {
      names(y)
    } else{
      NA
    }
  }
  myl <- as.list(apply(l, 1, gna))
  myl <- myl[!is.na(myl)]
  
  long <- sum(sapply(myl, length))
  df <- data.frame(
    id = rep(NA, long),
    id1 = rep(NA, long),
    id2 = rep(NA, long)
  )
  ind <- 1
  wkt <- rep(NA, long)
  for (i in 1:length(myl)) {
    id1 <- names(myl[i])
    li <- sldf[sldf$id == id1, ]
    for (j in 1:length(myl[[i]])) {
      id2 <- myl[[i]][[j]]
      po <- spdf[spdf$id == id2, ]
      Inter <- rgeos::gIntersection(li, po, byid = T)
      if (class(Inter) != "SpatialLines") {
        if (class(Inter) == "SpatialPoints") {
          rm(Inter)
        } else{
          Inter <- Inter@lineobj
          row.names(Inter) <- paste(id1, id2, sep = "_")
          wkt[ind] <- rgeos::writeWKT(Inter)
          df[ind,] <- c(paste(id1, id2, sep = "_"), id1, id2)
          ind <- ind + 1
        }
      } else{
        row.names(Inter) <- paste(id1, id2, sep = "_")
        wkt[ind] <- rgeos::writeWKT(Inter)
        df[ind,] <- c(paste(id1, id2, sep = "_"), id1, id2)
        ind <- ind + 1
      }
    }
  }
  wkt <- wkt[!is.na(wkt)]
  wkt <- paste("GEOMETRYCOLLECTION(",
               paste(wkt, collapse = ","), ")", sep = "")
  
  b <- readWKT(wkt, id = df$id)
  row.names(df) <- df$id
  b <- SpatialLinesDataFrame(sl = b,
                             data = df,
                             match.ID = T)
  b2 <- b
  b2@data <- b2@data[, c(1, 3, 2)]
  names(b2) <- c("id", "id1", "id2")
  b2$id <- paste(b2$id1, b2$id2, sep = "_")
  row.names(b2) <- b2$id
  borderlines <- rbind(b, b2)
  borderlines@proj4string <- sldf@proj4string
  
  borderlines <- sf::st_as_sf(borderlines)
  
  return(borderlines)
}
  
#   
#   if(missing(x)){
#     x <- sf::st_as_sf(spdf)
#   }
#   
#   if(is.null(id)){id <- names(x)[1]}
#   
#   st_geometry(x) <-  st_buffer(st_geometry(x), 1)
#   l <- st_intersects(x,x, sparse = F)
#   colnames(l) <- x[[id]]
#   rownames(l) <- x[[id]]
#   l <- lower.tri(l) * l
#   
#   gna <- function(x) {
#     y <- x[x == 1]
#     if (length(y) > 0) {
#       names(y)
#     } else {
#       NA
#     }
#   }
#   myl <- as.list(apply(l, 1, gna))
#   myl <- myl[!is.na(myl)]
#   long <- sum(sapply(myl, length))
#   df <- data.frame(id = rep(NA, long), id1 = rep(NA, long), id2 = rep(NA, long))
#   
#   lgeo <- vector(mode = "list", length = long)
#   lgeo2 <- vector(mode = "list", length = long)
#   ind <- 1
#   i<-1
#   for (i in 1:length(myl)) {
#     id1 <- names(myl[i])
#     li <- x[x[[id]] == id1, ]
#     j <- 1
#     for (j in 1:length(myl[[i]])) {
#       id2 <- myl[[i]][[j]]
#       po <- x[x[[id]] == id2, ]
#       Inter <- st_intersection(st_geometry(li), st_geometry(po))
#       df[ind,] <- c(paste0(id1,"_",id2), id1, id2)
#       lgeo[[ind]]   <- Inter[[1]]
#       ind <- ind + 1
#     }
#   }
#   df <- st_sf(df, geometry = st_sfc(lgeo))
#   df <-  st_cast(x = df, to = "MULTILINESTRING")
#   st_crs(df) <- st_crs(x)
#   
#   df2 <- df[, c(1,3,2)]
#   
#   names(df2) <- c("id", "id1", "id2", "geometry")
#   df2$id <- paste(df2$id1, df2$id2, sep = "_")
#   borderlines <- rbind(df, df2)
#   row.names(borderlines) <- borderlines$id
#   return(borderlines)
# }
