#' @title carroyage
#' @description Build a regular grid
#' @name carroyage
#' @details Build a regular grid from a SpatialPolygonsDataFrame and a table of surface intersection
#' @param spdf SpatialPolygonsDataFrame
#' @param celsize size of the cell
#' @param spdfid unique id on the spdf
#' @import sp
#' @import reshape2
#' @examples
#' data(nuts2006)
#' mygrid<-carroyage(spdf=nuts2.spdf,celsize=200000)
#' plot(mygrid$spdf)
#' head(mygrid$df)
#' @return SpatialLinesDataFrame
#' @export


carroyage <- function(spdf, celsize,spdfid=NULL){

  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("'rgeos' package needed for this function to work. Please install it.",
         call. = FALSE)
  }


if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}


spdf@data <- spdf@data[spdfid]
row.names(spdf@data)<-spdf@data[,spdfid]
spdf <- spChFIDs(spdf, spdf@data[,spdfid])
spdf@data$area <- rgeos::gArea(spdf, byid=TRUE)

extent <- bbox(spdf)
posx <- extent[1]
posy <- extent[2]
nbcells_x <- floor((extent[3] - extent[1]) / celsize)+1
nbcells_y <- floor((extent[4] - extent[2]) / celsize)+1

pb <- txtProgressBar(min = 0, max = nbcells_x, style = 3)
for(i in 0:nbcells_x){
  for(j in 0:nbcells_y){
  square <- paste("POLYGON((",posx+celsize*i," ",posy+celsize*j,",",posx+celsize+celsize*i," ",posy+celsize*j,",",posx+celsize+celsize*i," ",posy+celsize+celsize*j,",",posx+celsize*i," ",posy+celsize+celsize*j,",",posx+celsize*i," ",posy+celsize*j,"))",sep="")

  if(i==0 & j == 0){
    spgrid <- rgeos:: readWKT(square)
    spgrid <- spChFIDs(spgrid, iden <- paste(i,j,sep="_"))
    } else {
    spgrid2 <- rgeos:: readWKT(square)
    spgrid2 <- spChFIDs(spgrid2, iden <- paste(i,j,sep="_"))
    spgrid <- rbind(spgrid,spgrid2)
  }
  }
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb, i)
}
data<-data.frame(id=sapply(slot(spgrid, "polygons"), slot, "ID"))
row.names(data)<-data$id
spgrid<-SpatialPolygonsDataFrame(spgrid, data)
proj4string(spgrid) <-proj4string(spdf)

# On ne garde que ce qui touche le fond de carte initial
over <- rgeos::gDisjoint(spgrid, spdf, byid = TRUE)
head(over)
over <-melt(over,variable.name=1,value.name="touch", na.rm=TRUE)
over <- over[over$touch ==FALSE,2:3]
over$touch <- "ok"
colnames(over)<-c("id","touch")
spgrid@data <- data.frame(spgrid@data, touch=over[match(spgrid@data$id, over$id),"touch"])
spgrid <- spgrid[!is.na(spgrid@data$touch),1]

mask <- rgeos:: gBuffer(spdf, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
spgrid<- rgeos::gIntersection(spgrid, mask, byid=TRUE, id=as.character(spgrid@data$id), drop_lower_td=FALSE)
data<-data.frame(id=sapply(slot(spgrid, "polygons"), slot, "ID"))
row.names(data)<-data$id
spgrid<-SpatialPolygonsDataFrame(spgrid, data)
spgrid@data$cell_area <- rgeos::gArea(spgrid, byid=TRUE)
proj4string(spgrid) <-proj4string(spdf)

# On calcul la table de passage

# intersection
parts <- rgeos::gIntersection(spgrid, spdf, byid=TRUE,drop_lower_td=TRUE)
data<-data.frame(id=sapply(slot(parts, "polygons"), slot, "ID"))
tmp <- data.frame(do.call('rbind', (strsplit(as.character(data$id)," "))))
data$id1 <- as.vector(tmp$X1)
data$id2 <- as.vector(tmp$X2)
row.names(data)<-data$id
parts<-SpatialPolygonsDataFrame(parts, data)
proj4string(parts) <-proj4string(spdf)

# Part de surface intersectée
parts@data$area_part <- rgeos::gArea(parts, byid=TRUE)
parts@data <- data.frame(parts@data, area_full=spdf@data[match(parts@data$id2, spdf@data[,spdfid]),"area"])
parts@data$area_pct <- (parts@data$area_part/parts@data$area_full)*100
areas <- parts@data[,c("id1","id2","area_pct")]
colnames(areas) <- c("id_cell","id_geo","area_pct")

return(list(spdf=spgrid,df=areas))
}


#' @title gridCompute
#' @description regional data -> grid data
#' @name gridCompute
#' @details This function compute the data within the grid layer according to surface intersection
#' @param carroyage Carroyage object generated with the function carroyage()
#' @param df dataframe with numeric values
#' @param dfid unique id in the df
#' @param var statistic indicator (numeric)
#' @examples
#' data(nuts2006)
#' mygrid <- carroyage(nuts2.spdf,400000)
#' datagrid.df <- gridCompute(mygrid, nuts2.df, "pop2008",dfid=NULL)
#' plot(mygrid$spdf,col="#CCCCCC",border="white")
#' propSymbolsLayer(spdf= mygrid$spdf, df = datagrid.df, var = "pop2008",k=0.03,col="black")
#' @return dataframe
#' @export

gridCompute <- function(carroyage, df, dfid=NULL, var) {

  if (is.null(dfid)){dfid <- names(df)[1]}
  tab <- carroyage$df
  tab <- data.frame(tab, var=df[match(tab$id_geo, df[,dfid]),var])
  tab$var_parts <- (tab$var*tab$area_pct/100)

  # Construction du jeu de données : aggregate by id_cell
  grid_data<-aggregate(tab$var_parts, by = list(key = tab$id_cell), sum, simplify=TRUE)
  colnames(grid_data) <- c("id_cell","var")

  # Ajoutr d'une colonne de densité si on veut représenter en applat
  grid_data <- data.frame(grid_data, cell_area=carroyage$spdf@data[match(grid_data$id_cell, carroyage$spdf@data$id),"cell_area"])
  grid_data$var_density <- (grid_data$var/grid_data$cell_area)*1000
  grid_data <-grid_data[,c("id_cell","var","var_density")]
  colnames(grid_data) <- c("id_cell",var,paste(var,"dentisy",sep="_"))

  return(grid_data)

}

