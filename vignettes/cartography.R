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
# path to the geopackage file embedded in cartography
path_to_file <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
# plot municipalities
plot(st_geometry(mtq), col = "darkseagreen3", border = "darkseagreen4", 
     bg = "lightblue1", lwd = 0.5)
# plot labels
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
  title = "Municipalities of Martinique", 
  sources = "Sources: Insee and IGN - 2018",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = TRUE, 
  tabtitle = TRUE
) 



## ----choroMap, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_file <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
# population density (inhab./km2) using sf::st_area()
mtq$POPDENS <- 1e6 * mtq$POP / st_area(mtq)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# plot population density
choroLayer(
  x = mtq, 
  var = "POPDENS",
  method = "geom",
  nclass=5,
  col = carto.pal(pal1 = "sand.pal", n1 = 5),
  border = "grey40", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "Population Density\n(people per km2)",
  add = TRUE
) 
# http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?topicname=geometrical_interval
# layout
layoutLayer(title = "Population Distribution in Martinique", 
            sources = "Sources: Insee and IGN - 2018",
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, north = FALSE, tabtitle = TRUE) 
# north arrow
north(pos = "topleft")


## ----propMap, fig.height=6, fig.width=5, message=FALSE, margin=TRUE------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_file <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
# # Plot OpenStreetMap tiles as basemap
# download osm tiles
mtq.osm <- getTiles(
  x = mtq, 
  type = "osm", 
  zoom = 11, 
  crop = TRUE
)
# plot osm tiles
tilesLayer(x = mtq.osm)
# plot municipalities (only borders are plotted)
plot(st_geometry(mtq), col = NA, border = "grey", add=FALSE)
# plot population
propSymbolsLayer(x = mtq, var = "POP", inches = 0.4, col = "brown4",
  legend.pos = "topright",  legend.title.txt = "Total population")
# layout
layoutLayer(title = "Population Distribution in Martinique",
            sources = "Sources: Insee and IGN - 2018\n© OpenStreetMap contributors.\nTiles style under CC BY-SA, www.openstreetmap.org/copyright.",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "topleft")

## ----isopleth, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_file <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# plot isopleth map
smoothLayer(
  x = mtq, 
  var = 'POP',
  span = 4000,
  nclass = 12,
  beta = 2,
  mask = mtq, 
  border = "grey",
  lwd = 0.1, legend.values.rnd = -3,
  col = carto.pal(pal1 = 'brown.pal', n1 = 12),
  legend.title.txt = "Population\nPotential",
  legend.pos = "topright", add=TRUE
)
# annotation on the map
text(x = 692582, y = 1611478, cex = 0.8, adj = 0, font = 3,  labels = 
       "Distance function:\n- type = exponential\n- beta = 2\n- span = 4 km")
# layout
layoutLayer(title = "Population Distribution in Martinique",
            sources = "Sources: Insee and IGN - 2018",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "topleft")


## ----grid, fig.height=6, fig.width=5, margin=TRUE------------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_file <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)

# Create a grid layer
mygrid <- getGridLayer(
  x = mtq, 
  cellsize = (median(st_area(mtq))), 
  var = c("POP"),
  type = "hexagonal"
)
# Compute population density in people per km2
mygrid$POPDENS <- 1e6 * mygrid$POP / mygrid$gridarea
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# Plot the population density
choroLayer(x = mygrid, var = "POPDENS", method = "geom", nclass=5, 
           col = carto.pal(pal1 = "turquoise.pal", n1 = 5), border = "grey80", 
           lwd = 0.5, legend.pos = "topright", add = TRUE,
           legend.title.txt = "Population Density\n(people per km2)") 
layoutLayer(title = "Population Distribution in Martinique", 
            sources = "Sources: Insee and IGN - 2018",
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, north = FALSE, tabtitle = TRUE) 

## ----discc, fig.height=5, fig.width=5, margin=TRUE-----------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_file <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)
# Compute the population density (inhab./km2) using sf::st_area()
mtq$POPDENS <- as.numeric(1e6 * mtq$POP / st_area(mtq))
# Get a SpatialLinesDataFrame of countries borders
mtq.contig <- getBorders(mtq)

plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1", 
     xlim = c(690574, 745940))
# Plot the population density
choroLayer(x = mtq, var = "MED",
           breaks = c(min(mtq$MED), seq(13000, 21000, 2000), max(mtq$MED)),
           col = carto.pal("green.pal", 6),border = "white", lwd = 0.5, 
           legend.pos = "topright", legend.title.txt = "Median Income\n(euros)",
           add = TRUE)

# Plot discontinuities
discLayer(
  x = mtq.contig, 
  df = mtq, 
  var = "MED",
  type = "rel", 
  method = "geom", 
  nclass = 3,
  threshold = 0.4,
  sizemin = 0.7, 
  sizemax = 6, 
  col = "red4",
  legend.values.rnd = 1, 
  legend.title.txt = "Relative\nDiscontinuities", 
  legend.pos = "right",
  add = TRUE
)

# Layout
layoutLayer(title = "Wealth Disparities in Martinique, 2015", 
            author =  paste0("cartography ", packageVersion("cartography")),
            sources = "Sources: Insee and IGN - 2018",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "grey.pal")


## ----sp, fig.height=5, fig.width=7, margin=TRUE--------------------------
library(sp)
library(cartography)
data("nuts2006")


# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")
# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border = NA, add = TRUE)
# Plot Nuts2 regions
plot(nuts0.spdf, col = "grey60",border = "white", lwd = 0.4, add = TRUE)

propSymbolsLayer(
  spdf = nuts0.spdf, 
  df = nuts0.df, 
  spdfid = "id", 
  dfid = "id", 
  var = "pop2008", 
  legend.pos = "topright", 
  col = "red4", 
  border = "white", 
  legend.title.txt = "Population" 
)

layoutLayer(title = "Population in Europe, 2008",
            sources = "Data: Eurostat, 2008",
            author =  paste0("cartography ", packageVersion("cartography")),
            scale = 500, frame = TRUE, col = "#688994") 

north("topleft")

