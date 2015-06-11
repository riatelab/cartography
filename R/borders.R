#' @title polygons to borders
#' @description between polygons
#' @name borders
#' @details Extract borders from a SpatialPolygonsDataFrame
#' @param spdf SpatialPolygonsDataFrame
#' @param tol tolerance to detect contiguity (in map units)
#' @param id unique id on the spdf
#' @import sp
#' @import reshape2
#' @examples
#' data(nuts2006)
#' plot(nuts0.spdf ,col="#CCCCCC",border="white",lwd=1)
#' contig <-borders(spdf=nuts0.spdf)
#' plot(contig,col="red",lwd=2,add=TRUE)
#' @return SpatialLinesDataFrame
#' @export

borders <- function(spdf, tol=1,id=NULL){

  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("'rgeos' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:rgeos' %in% search()){
    attachNamespace('rgeos')
  }
  if (is.null(id)){id <- names(spdf@data)[1]}

  # Distance ) tolerance /2
  distance <- tol/2
  mysep <- "_ksfh88ql_"

  # simplification du fichier, regroupement des polygones multi-parties
  spdf@data <- spdf@data[id]
  colnames(spdf@data)[1]<-"id"
  class(spdf@data$id)

  spdf@data$id <- as.vector(as.character(spdf@data$id))
  spdf <- sp::spChFIDs(spdf,spdf$id)
  data<-data.frame(id=sapply(slot(spdf, "polygons"), slot, "ID"))
  row.names(data)<-data$id
  spdf<-SpatialPolygonsDataFrame(spdf, data)

  # Sous fonction qui calcule les bordures 2 à 2 ----------------------------
  poly1xpoly2 <- function(spdf, distance, poly1, poly2){


    # buffers des pays
    geombuff <- rgeos::gBuffer(spdf, byid=TRUE, width=distance,quadsegs=1,capStyle="SQUARE")

    # Intersection
    tmp1 <- geombuff[geombuff@data$id==poly1,]
    tmp2 <- geombuff[geombuff@data$id==poly2,]
    frontArea <- rgeos::gIntersection(tmp1, tmp2)
    if(class(frontArea)=="SpatialPolygons"){
      iden <- paste(poly1,poly2,sep=mysep)
      frontArea <- sp::spChFIDs(frontArea,iden)
      return(frontArea)
    }
  }
  # fin de la sousfonction   ----------------------------


  # Execution de la sous fonction pour toutes les geométries contigues (selon la tol)

  geombuff <- rgeos::gBuffer(spdf, byid=TRUE, width=distance,quadsegs=1,capStyle="SQUARE")
  test <- rgeos::gDisjoint(geombuff, byid = TRUE)
  transfrontCountries<-melt(test,variable.name=1,value.name="fij", na.rm=TRUE)
  transfrontCountries <- transfrontCountries[transfrontCountries$Var1 !=transfrontCountries$Var2,]
  transfrontCountries <- transfrontCountries[transfrontCountries$fij ==FALSE,]

  bordures=NULL
  t <- 0
  pb <- txtProgressBar(min = 0, max = dim(transfrontCountries)[1], style = 3)
  for ( i in 1:(dim(transfrontCountries)[1])) {
    p1 <- as.character(transfrontCountries$Var1[i])
    p2 <- as.character(transfrontCountries$Var2[i])
    x<-poly1xpoly2(spdf=spdf,distance=distance,poly1=p1,poly2=p2)

    if(class(x)=="SpatialPolygons"){
      if(t==0){bordures<-x} else { bordures <-rbind(bordures, x) }
      t=1
    }
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)


  data<-data.frame(id=sapply(slot(bordures, "polygons"), slot, "ID"))

  row.names(data)<-data$id
  bordures<-SpatialPolygonsDataFrame(bordures, data)

  borders <- rgeos::gBoundary(bordures, byid=TRUE, id = bordures@data$id)
  borders<-SpatialLinesDataFrame(borders, data)

  borders@data
  tmp <- data.frame(do.call('rbind', strsplit(as.character(borders@data$id),mysep)))
  tmp$X1<-as.vector(tmp$X1)
  tmp$X2<-as.vector(tmp$X2)
  colnames(tmp)<-c("id1","id2")
  borders@data$id1 <- tmp$id1
  borders@data$id2 <- tmp$id2
  borders@data$id <- paste(borders@data$id1,borders@data$id2,sep="_")
  row.names(borders@data) <- borders@data$id
  borders <- sp::spChFIDs(borders,as.character(borders$id))
  return(borders)
}

