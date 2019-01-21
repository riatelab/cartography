## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

knitr::knit_hooks$set(margin = function(before, options, envir){
  if (before){
    par(mar=c(0.1,0.1,1.3,0.1))
  } 
})

## ----labelMap, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the ESRI Shapefile embedded in cartography
path_to_file <- system.file("shape/martinique.shp", package="cartography")
# import the Shapefile to a sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
# plot communes
plot(
  st_geometry(mtq), 
  col = "darkseagreen3", 
  border = "darkseagreen4", 
  bg = "lightblue1", 
  lwd = 0.5
)
# Label plot of the communes names
labelLayer(
  x = mtq, 
  txt = "LIBGEO", 
  col= "black", 
  cex = 0.7, 
  font = 4,
  halo = TRUE, 
  bg = "white", 
  r = 0.1, 
  overlap = FALSE, 
  show.lines = FALSE
)
# map layout
layoutLayer(
  title = "Communes of Martinique", 
  sources = "INSEE 2016",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = TRUE, 
  tabtitle = TRUE
) 



## ----choroMap, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the ESRI Shapefile embedded in cartography
path_to_file <- system.file("shape/martinique.shp", package="cartography")
# import the Shapefile to a sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
# Compute the population density (inhab./km2) using sf::st_area()
mtq$POP_DENS <- 1000*1000 * mtq$P13_POP / st_area(mtq)

# Set a custom color palette
cols <- carto.pal(pal1 = "sand.pal", n1 = 5)
# plot communes (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# Plot the population density
choroLayer(
  x = mtq, 
  var = "POP_DENS",
  method = "geom",
  nclass=5,
  col = cols,
  border = "grey40", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "Population Density\n(people per km2)",
  add = TRUE
) 
# http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?topicname=geometrical_interval
layoutLayer(
  title = "Population Distribution in Martinique", 
  sources = "INSEE 2016",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = FALSE, 
  tabtitle = TRUE
) 

north(pos = "topleft")


## ----propMap, fig.height=6, fig.width=5, message=FALSE, margin=TRUE------
## Plot OpenStreetMap tiles as basemap
# Download the tiles, nuts0.spdf extent
# mtq.osm <- getTiles(x = mtq, type = "osm", zoom = 11, crop = TRUE)
# # Plot the tiles
# tilesLayer(mtq.osm)
# plot communes (only borders are plotted)
plot(st_geometry(mtq), col = NA, border = "grey", add=FALSE)
# Plot communes population
propSymbolsLayer(
  x = mtq,
  var = "P13_POP",
  inches = 0.4,
  symbols = "circle",
  col = "brown4",
  legend.pos = "topright",
  legend.title.txt = "Total population"
)

layoutLayer(
  title = "Population Distribution in Martinique",
  sources = "INSEE 2016 - © OpenStreetMap contributors.\nTiles style under CC BY-SA, www.openstreetmap.org/copyright.",
  author = paste0("cartography ", packageVersion("cartography")),
  frame = FALSE,
  north = FALSE,
  tabtitle = TRUE
)

north(pos = "topleft")

## ----isopleth, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the ESRI Shapefile embedded in cartography
path_to_file <- system.file("shape/martinique.shp", package="cartography")
# import the Shapefile to a sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
options(scipen = 6)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
smoothLayer(
  x = mtq, 
  var = 'P13_POP',
  span = 4000, 
  beta = 2, 
  breaks = c(0,5000,seq(10000,100000,10000),105700 ),
  mask = mtq, 
  border = "grey",
  lwd = 0.2,
  col = carto.pal(pal1 = 'brown.pal', n1 = 12),
  legend.title.txt = "Population\nPotential",
  legend.pos = "topright", add=TRUE
)
text(
  x = 692582, y = 1611478, 
  labels = "Distance function:\n- type = exponential\n- beta = 2\n- span = 4 km", 
  cex = 0.8, adj = 0, font = 3
)
layoutLayer(
  title = "Population Distribution in Martinique",
  sources = "INSEE 2016",
  author = paste0("cartography ", packageVersion("cartography")),
  frame = FALSE,
  north = FALSE,
  tabtitle = TRUE
)

north(pos = "topleft")


## ----grid, fig.height=4.33, fig.width=5, margin=TRUE---------------------
library(sf)
library(cartography)
# path to the ESRI Shapefile embedded in cartography
path_to_file <- system.file("shape/martinique.shp", package="cartography")
# import the Shapefile to a sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)

# Create a grid layer
mygrid <- getGridLayer(
  x = mtq, 
  cellsize = (median(st_area(mtq))), 
  var = c("C13_POP"),
  type = "hexagonal"
)

# Compute population density in people per km2
mygrid$POP_DENS <- 1e6 * mygrid$C13_POP / mygrid$gridarea

# Set a custom color palette
cols <- carto.pal(pal1 = "sand.pal", n1 = 5)
# plot communes (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# Plot the population density
choroLayer(
  x = mygrid, 
  var = "POP_DENS",
  method = "geom",
  nclass=5,
  col = cols,
  border = "grey80", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "Population Density\n(people per km2)",
  add = T
) 
layoutLayer(
  title = "Population Distribution in Martinique", 
  sources = "INSEE 2016",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = FALSE, 
  tabtitle = TRUE
) 



## ----discc, fig.height=4.33, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the ESRI Shapefile embedded in cartography
path_to_file <- system.file("shape/martinique.shp", package="cartography")
# import the Shapefile to a sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)

# Compute the population density (inhab./km2) using sf::st_area()
mtq$POP_DENS <- as.numeric(1e6 * mtq$P13_POP / st_area(mtq))

# Get a SpatialLinesDataFrame of countries borders
mtq.contig <- getBorders(mtq)

plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# Plot the population density
choroLayer(
  x = mtq, 
  var = "POP_DENS",
  method = "geom",
  nclass=5,
  col = cols,
  border = "grey80", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "Population Density\n(people per km2)",
  add = TRUE
)

# Plot discontinuities
discLayer(
  x = mtq.contig, 
  df = mtq, 
  var = "POP_DENS",
  type = "rel", 
  method = "geom", 
  nclass = 3,
  threshold = 0.6,
  sizemin = 0.7, 
  sizemax = 6, 
  col = "red",
  legend.values.rnd = 1, 
  legend.title.txt = "Discontinuities in \nGDP per Capita\n(relative)", 
  legend.pos = "bottomright",
  add = TRUE
)

# Layout
layoutLayer(
  title = "Wealth Disparities in Europe, 2008", 
  author =  paste0("cartography ", packageVersion("cartography")),
  sources = "Source: Eurostat, 2011", 
  frame = FALSE, 
  scale = 500, 
  theme = "grey.pal"
)


