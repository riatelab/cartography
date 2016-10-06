# spdf the Spatial*Dataframe to resize and move
# bb bbox of the destination, spdf will move inside that bbox. If bb is a Spatial*DataFrame, it will use its bbox. k and xy are not used. 
# prj projection of the output geometry. Must be a valid proj4 string
# mask mask used to clip the initial geometry before move and resize
# k factor of extension for the resize process, bb is not used.
# xy coordinates used for the move process, bb is not used. 

library(maptools)
library(rgeos)
data(nuts2006)
library(rgdal)
country <- readOGR(dsn = "/data/dta/world/", layer = "country")
graticule <- readOGR(dsn = "/data/dta/world/", layer = "graticule")


country <- spTransform(country[country$ISO2!="AQ",], CRSobj = "+init=epsg:3395")

plot(country)

locator(2)
mask <- rgeos::readWKT("POLYGON((-14135999 1647834, 
                              -14135999 7061414,
                              -10213115 7061414, 
                              -10213115 1647834, 
                              -14135999 1647834))")


plot(mask, add=T)
proj4string(mask) <- proj4string(country)
x <- move_and_resize(spdf = country, bb = mask, xy = xy, k = 0.2)



data(nuts2006)
nuts0.spdf@data <- nuts0.df
spdf <- nuts0.spdf[nuts0.spdf$id %in% c("FR", "LU", "DE", "BE"), ]
spdf <- nuts0.spdf[nuts0.spdf$id %in% c("DE", "LU"), ]
mask <- rgeos::readWKT("POLYGON((3862526 2770745, 
                              3862526 2971831,
                              4200108 2971831, 
                              4200108 2770745, 
                              3862526 2770745))")
proj4string(mask) <- proj4string(spdf)

bb <- mask
k <- 3
xy <- c(5605177, 4914844)


x <- move_and_resize(spdf = spdf, bb = bb, xy = xy, k = 1)

plot(nuts0.spdf)
plot(x, add=T, col = "green")

points(xy[1],xy[2])

x <- move_and_resize(spdf = country, bb = mask, xy = xy, k = 0.2, prj = proj4string(nuts0.spdf))

x@data <- data.frame(id=x@data$ISO2,row.names = row.names(x))
x@proj4string

nx <- rbind(nuts0.spdf, x)

plot(nx)

nx.df <- rbind(nuts0.df, c("US", 29363.900 ,31116.400  ,1752.500   ,  794383 ,    579697 ,   1231099 ,   1763903, 58637924, 61192129))
nuts0.df[35, ] <- c("US", 29363.900 ,31116.400  ,1752.500   ,  794383 ,    579697 ,   1231099 ,   1763903, 58637924, 61192129)

library(cartography)
nuts0.df$xx <- as.numeric(as.character(nuts0.df$pop2008))
choroLayer(spdf = nx, df = nuts0.df, var = "xx")
str(nx.df)
nx@data

row.names(x)
row.names(x@data)


nuts0.spdf@data


x <- move_and_resize(spdf = nuts0.spdf[nuts0.spdf$id=="FR", ], mask = NULL, xy = xy, k = 0.2)


move_and_resize <- function(spdf, mask, xy, prj = proj4string(spdf), k){
  # A faire fonction pour transformer une bbox en spdf
  # argument pos pour l'encrage du carton
  # gUnaryNunionage du mask si nrow > 1
  if (is.null(mask)){
    mask <- gBuffer(spdf)
    
  }
  if(length(mask) > 1 ){
    mask <- rgeos::gBuffer(mask)
  }

  # proj4string(mask) <- proj4string(spdf)
  
  
  inter <- gIntersection(mask, spdf, byid = T)

  df <- data.frame(id = sapply(methods::slot(inter, "polygons"), 
                           methods::slot, "ID"))
  df <- data.frame(do.call('rbind', (strsplit(as.character(df$id)," "))))
  row.names(df) <- df$X2
  df <- data.frame(spdf@data[match(row.names(df),  row.names(spdf) ), ])
  row.names(inter) <- row.names(df)
  spdf <- SpatialPolygonsDataFrame(Sr = inter, data = df, match.ID = T)

  x <- spdf[1, ]
  x@polygons <- mask@polygons
  x@polygons[[1]]@ID <- "YOLO"
  row.names(x@data) <- row.names(x)
  spdf <- rbind(x, spdf)
  

  spdf_bb <- bbox(spdf) 
  spdf_w <- spdf_bb[1,2] - spdf_bb[1,1]
  spdf_h <- spdf_bb[2,2] - spdf_bb[2,1]
  spdf_sizemax <- which.max(c(spdf_w, spdf_h))
  
  
  if(spdf_sizemax==1){
    scale <- spdf_w * k
  }else{
    scale <- spdf_h * k
  }
  
  if(k==1){
    spdf_resized <- spdf
  }else{
    spdf_resized <- elide(obj = spdf, scale = scale)
  }
  # points(x = xy[1], y = xy[2], cex = 2, col = "red", pch = 20)
  
  spdf_moved <- elide(obj = spdf_resized,  shift=xy)
  
  spdf_moved <- spdf_moved[-1, ]
  proj4string(spdf_moved) <- prj
  
  return(spdf_moved)
  
}
  

plot(nuts0.spdf)
plot(spdf_moved, add=T, col = "red")
