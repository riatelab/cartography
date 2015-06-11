# ----------------------------------------------
# CLASSE : PropSymbolsDuo
# DESCRIPTION: Build thematic maps with TWO proportional symbols
# AUTHOR(s) : Nicolas LAMBERT / CNRS / UMS RIATE
# EMAIL(s) : nicolas.lambert@ums-riate.fr
# LAST REVISION: oct 2014
# ----------------------------------------------

#library ("maptools")

# TODO TO IMPROVE
# 

setClass (
  Class = "PropSymbolsDuo" ,
  slots = c(
    geom = "Spatial", # basemap    
    data = "data.frame", # data
    geomId = "character", 
    geomProj = "character", 
    dataId = "character",
    dataField1 = "character",
    dataField2 = "character",
    col = "character",
    col2="character",
    breakVal="numeric",
    nbCols="numeric",
    type = "character",
    k="numeric",
    fixMax="logical",
    test="character",
    add="logical",
    size="numeric",
    var="numeric",
    legPos="character",
    legTitle="character"
  ),
  prototype=list(
    type="circles",
    col="#E84923",
    col2="#7DC437",
    geomId="undefined",
    dataId="undefined",
    breakVal=0,
    k=0.2,
    title="Title",
    legPos="bottomleft",
    legTitle="",
    nbCols<-1,
    add=F,
    fixMax=F
  )
)




setGeneric(
  name = "AddPropSymbolsDuo" ,
  def=function (object){ standardGeneric ("AddPropSymbolsDuo")
  }
)

setMethod("AddPropSymbolsDuo","PropSymbolsDuo",
          function (object){
            
if (object@geomId=="undefined"){object@geomId<-names(object@geom@data)[1]}
if (object@dataId=="undefined"){object@dataId<-names(object@data)[1]}    
                                
# TRIANGLE TOP
dots <- cbind(object@geom@data[,object@geomId],as.data.frame(coordinates(object@geom)))
colnames(dots) <- c(object@geomId,"x","y")  
dots <- data.frame(dots, object@data[match(dots[,object@geomId], object@data[,object@dataId]),])
dots$size1 <-  sqrt(dots[,object@dataField1]*object@k)
dots$y1<-dots$y+dots$size1/2
dots <- dots[order(dots[,object@dataField1],decreasing=TRUE),] 
dots$xx1a<-dots$x-dots$size1/2
dots$xx1b<-dots$x
dots$xx1c<-dots$x+dots$size1/2
dots$yy1a<-dots$y
dots$yy1b<-dots$y+dots$size1/2
dots$yy1c<-dots$y
for (i in 1:length(dots$x)){polygon(c(dots$xx1a[i],dots$xx1b[i],dots$xx1c[i]), c(dots$yy1a[i],dots$yy1b[i],dots$yy1c[i]), col = object@col, border = "#DDDDDD")}

# TRIANGLE BOTTOM
dots <- cbind(object@geom@data[,object@geomId],as.data.frame(coordinates(object@geom)))
colnames(dots) <- c(object@geomId,"x","y")  
dots <- data.frame(dots, object@data[match(dots[,object@geomId], object@data[,object@dataId]),])
dots$size2 <-  sqrt(dots[,object@dataField2]*object@k)
dots$y1<-dots$y+dots$size2/2
dots <- dots[order(dots[,object@dataField2],decreasing=TRUE),] 
dots$xx1a<-dots$x-dots$size2/2
dots$xx1b<-dots$x
dots$xx1c<-dots$x+dots$size2/2
dots$yy1a<-dots$y
dots$yy1b<-dots$y-dots$size2/2
dots$yy1c<-dots$y
for (i in 1:length(dots$x)){polygon(c(dots$xx1a[i],dots$xx1b[i],dots$xx1c[i]), c(dots$yy1a[i],dots$yy1b[i],dots$yy1c[i]), col = object@col2, border = "#DDDDDD")}


            
          
          }
)