#' @title Extract SpatialPolygonsDataFrame Borders
#' @description Extract borders between SpatialPolygonsDataFrame units.
#' @name getBorders
#' @param spdf a SpatialPolygonsDataFrame. This SpatialPolygonsDataFrame
#'  has to be projected (planar coordinates).
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param tol tolerance to detect contiguity (in map units). You may 
#' not want to change this parameter.
#' @return A SpatialLinesDataFrame of borders is returned. This object has three 
#' id fields: id, id1 and id2.
#' id1 and id2 are ids of units that neighbour a border; id is the concatenation 
#' of id1 and id2 (with "_" as separator).
#' @note This function uses the rgeos package.
#' @import sp
#' @examples
#' data(nuts2006)
#' # Get units borders
#' nuts0.contig.spdf <- getBorders(nuts0.spdf)
#' # Random colors
#' nuts0.contig.spdf$col <- sample(x = rainbow(length(nuts0.contig.spdf)))
#' # Plot Countries
#' plot(nuts0.spdf, border = NA, col = "grey60")
#' # Plot borders
#' plot(nuts0.contig.spdf, col = nuts0.contig.spdf$col, lwd = 3, add = TRUE)
#' @seealso \link{discLayer}
#' @export
getBorders <- function(spdf, spdfid = NULL, tol = 1){
  # Package check and loading
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("'rgeos' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:rgeos' %in% search()){
    attachNamespace('rgeos')
  }
  
  # Distance : tolerance /2
  distance <- tol/2
  mysep <- "_ksfh88ql_"
  
  # create comments for polygons with holes
  spdf <- rgeos::createSPComment(sppoly = spdf)
  
  # spdf and spdfid check
  id <- spdfid
  if (is.null(id)){id <- names(spdf@data)[1]}
  spdf@data <- spdf@data[id]
  colnames(spdf@data)[1]<-"id"
  row.names(spdf) <- as.character(spdf@data$id)
  
  # Create a Buffer around polygons
  geombuff <- rgeos::gBuffer(spdf, byid = TRUE, width = distance, quadsegs = 1, 
                             capStyle = "SQUARE")
  
  # Create intersecions table between polygons
  intergeom <- rgeos::gIntersects(geombuff, byid = TRUE, returnDense = F)
  b1 <- length(intergeom)
  t <- 0
  
  for (i in 1:b1) {
    # Intersection
    tmp1 <- geombuff[geombuff@data$id==names(intergeom[i]),]
    for (j in intergeom[[i]]){
      if (i != j){
        # create a spdf for each intersection
        tmp2 <- geombuff[j,]
        frontArea <- rgeos::gIntersection(tmp1, tmp2)
        row.names(frontArea) <- paste(tmp1@data$id,tmp2@data$id,sep=mysep)
        if(class(frontArea)=="SpatialPolygons"){
          if(t==1){
            borders <- rbind(borders, frontArea)
          } else { 
            borders <- frontArea
            t <- 1
          }
        }
      }
    }
  }
  
  # From spatialpolygonsdataframe to spatiallinesdataframe  
  df <- data.frame(id = sapply(methods::slot(borders, "polygons"), 
                               methods::slot, "ID"))
  row.names(df) <- df$id
  borders <- SpatialPolygonsDataFrame(Sr = borders, data = df)
  bordersline <- rgeos::gBoundary(borders, byid=TRUE, id = borders@data$id)
  bordersline <- SpatialLinesDataFrame(bordersline, df)
  
  # Ids management
  bordersline@data <- data.frame(
    do.call('rbind',strsplit(as.character(bordersline@data$id), mysep)))
  colnames(bordersline@data) <- c("id1","id2")
  bordersline@data$id <- paste(bordersline@data$id1, 
                               bordersline@data$id2,sep="_")
  row.names(bordersline@data) <- bordersline@data$id
  bordersline@data <- bordersline@data[, c("id", "id1", "id2")] 
  return(bordersline)
}