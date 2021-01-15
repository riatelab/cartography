#' @title Get borders from polygons
#' @description Extract borders between polygons.
#' @name tc_get_borders
#' @eval my_params("x")
#' @param id name of the identifier variable in x, default to the first column
#' @param outer if TRUE outer borders (non-contiguous polygons borders like
#' maritime borders) are computed.
#' @param res resolution of the grid used to compute outer borders (in x units).
#' A high resolution will give more detailed borders. (optional)
#' @param width maximum distance between used to compute outer borders (in x units).
#' A higher width will build borders between units that are farther apart. (optional)
#' @return An sf object (MULTILINESTRING) of borders is returned. This object
#' has three id variables: id, id1 and id2.
#' id1 and id2 are ids of units that neighbour a border; id is the concatenation
#' of id1 and id2 (with "_" as separator).
#' @examples
#' mtq <- tc_import_mtq()
#' mtq_borders <- tc_get_borders(x = mtq)
#' plot(mtq_borders)
#' @export
tc_get_borders <- function(x, id, outer = FALSE, res = NULL, width = NULL) {
  
  if (missing(id)) {
    id <- names(x)[1]
  }

  if(outer){
    return(get_ob(x = x, id = id, res = res, width = width))
  }
  
  
  st_geometry(x) <- st_buffer(x = st_geometry(x), 1, nQuadSegs = 5)
  lx <- st_cast(x, "MULTILINESTRING")

  l <- st_intersects(x, x, sparse = FALSE)
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
  df <- data.frame(
    id = rep(NA, long),
    id1 = rep(NA, long),
    id2 = rep(NA, long)
  )

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
      df[ind, ] <- c(paste0(id1, "_", id2), id1, id2)
      lgeo[[ind]] <- Inter[[1]]
      ind <- ind + 1
    }
  }

  df <- st_sf(df, geometry = st_sfc(lgeo))
  df <- st_cast(x = df, to = "MULTILINESTRING")
  st_set_crs(df, st_crs(x))

  df2 <- df[, c(1, 3, 2)]

  names(df2) <- c("id", "id1", "id2", "geometry")
  df2$id <- paste(df2$id1, df2$id2, sep = "_")
  borderlines <- rbind(df, df2)
  row.names(borderlines) <- borderlines$id
  return(borderlines)
}



get_ob <- function(x, id, res = NULL, width = NULL){

  if(methods::is(x,'sf')){
      spdf <- methods::as(x, "Spatial")
  }
  
  if (missing(id)) {
    id <- names(spdf@data)[1]
  }
  
  if(!is.numeric(spdf@data[,id])){
    spdf$idxd <- 1:nrow(spdf)
  }else{
    spdf$idxd <- spdf@data[, id]
  }
  
  
  
  boundingBox <- sp::bbox(spdf)
  w <- (boundingBox[1,2] - boundingBox[1,1])
  h <- (boundingBox[2,2] - boundingBox[2,1])
  
  
  if(is.null(res)){
    res <- round(max(c(w, h)) / 150, 0)
  }
  
  if(is.null(width)){
    width <- round(max(c(w, h)) / 20, 0)
  }
  
  
  # Create raster of spdf
  ex <- raster::extent(spdf)
  ex[1] <- ex[1] - width
  ex[2] <- ex[2] + width
  ex[3] <- ex[3] - width
  ex[4] <- ex[4] + width
  
  r <- raster::raster(ex, resolution = res)
  
  r <- raster::rasterize(x = spdf, y = r, field = 'idxd')
  
  dist <- raster::distance(r)
  dist[dist > width] <- NA
  # you can also set a maximum distance: dist[dist > maxdist] <- NA
  direct <- raster::direction(r, from = FALSE)
  
  # NA raster
  rna <- is.na(r) # returns NA raster
  
  # store coordinates in new raster: http://stackoverflow.com/a/35592230/3752258
  na.x <- raster::init(rna, 'x')
  na.y <- raster::init(rna, 'y')
  
  
  # calculate coordinates of the nearest Non-NA pixel
  # assume that we have a orthogonal, projected CRS, 
  # so we can use (Pythagorean) calculations
  co.x <- na.x + dist * sin(direct)
  co.y <- na.y + dist * cos(direct)
  
  # matrix with point coordinates of nearest non-NA pixel
  co <- cbind(co.x[], co.y[])
  
  
  # extract values of nearest non-NA cell with coordinates co
  NAVals <- raster::extract(r, co, method='simple')
  r.NAVals <- rna # initiate new raster
  r.NAVals[] <- NAVals # store values in raster
  
  pB <- raster::rasterToPolygons(r.NAVals, dissolve = TRUE)
  pB@data$id <- spdf@data[pB@data$layer, id]
  
  
  pBBorder <- getBorders(x = pB, id = "id" )
  result <- sf::st_simplify(x=pBBorder, dTolerance = res, 
                            preserveTopology = FALSE)
  row.names(result) <- paste0(row.names(result), "_o")
  
  
  
  return(result)
}

