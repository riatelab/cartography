## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

knitr::knit_hooks$set(margin = function(before, options, envir){
  if (before){
    par(mar=c(0.1,0.1,1.3,0.1))
  } 
})

## ----propMap, fig.height=6, fig.width=5, message=FALSE, margin=TRUE------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
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
plot(st_geometry(mtq), col = NA, border = "grey", add=TRUE)
# plot population
propSymbolsLayer(
  x = mtq, 
  var = "POP", 
  inches = 0.4, 
  col = "brown4",
  legend.pos = "topright",  
  legend.title.txt = "Total population"
)
# layout
layoutLayer(title = "Population Distribution in Martinique",
            sources = "Sources: Insee and IGN, 2018\n© OpenStreetMap contributors.\nTiles style under CC BY-SA, www.openstreetmap.org/copyright.",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "topleft")

## ----choroMap, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# population density (inhab./km2) using sf::st_area()
mtq$POPDENS <- 1e6 * mtq$POP / st_area(mtq)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "#aadaff")
# plot population density
choroLayer(
  x = mtq, 
  var = "POPDENS",
  method = "geom",
  nclass=5,
  col = carto.pal(pal1 = "sand.pal", n1 = 5),
  border = "white", 
  lwd = 0.5,
  legend.pos = "topright", 
  legend.title.txt = "Population Density\n(people per km2)",
  add = TRUE
) 
# layout
layoutLayer(title = "Population Distribution in Martinique", 
            sources = "Sources: Insee and IGN, 2018",
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, north = FALSE, tabtitle = TRUE, theme= "sand.pal") 
# north arrow
north(pos = "topleft")

## ----pentypoMap, fig.height=6, fig.width=5, margin=TRUE------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# transform municipality multipolygons to (multi)linestrings
mtq_pencil <- getPencilLayer(
  x = mtq, 
  size = 400, 
  lefthanded = F
)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = "white", border = NA, bg = "lightblue1")
# plot administrative status
typoLayer(
  x = mtq_pencil, 
  var="STATUS",  
  col = c("aquamarine4", "yellow3","wheat"), 
  lwd = .7,
  legend.values.order = c("Prefecture",
                          "Sub-prefecture", 
                          "Simple municipality"),
  legend.pos = "topright",
  legend.title.txt = "", 
  add = TRUE
)
#  plot municipalities
plot(st_geometry(mtq), lwd = 0.5, border = "grey20", add = TRUE, lty = 3)
# labels for a few  municipalities
labelLayer(x = mtq[mtq$STATUS != "Simple municipality",], txt = "LIBGEO", 
           cex = 0.9, halo = TRUE, r = 0.15)
# title, source, author
layoutLayer(title = "Administrative Status",
            sources = "Sources: Insee and IGN, 2018", 
            author = paste0("cartography ", packageVersion("cartography")), 
            north = FALSE, tabtitle = TRUE, postitle = "right", 
            col = "white", coltitle = "black") 
# north arrow
north(pos = "topleft")

## ----propchoro, fig.height=6, fig.width=5, margin=TRUE-------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# Plot the municipalities
plot(st_geometry(mtq), col="darkseagreen3", border="darkseagreen4",  
     bg = "lightblue1", lwd = 0.5)
# Plot symbols with choropleth coloration
propSymbolsChoroLayer(
  x = mtq, 
  var = "POP", 
  inches = 0.4,
  border = "grey50",
  lwd = 1,
  legend.var.pos = "topright", 
  legend.var.title.txt = "Population",
  var2 = "MED",
  method = "equal", 
  nclass = 4, 
  col = carto.pal(pal1 = "sand.pal", n1 = 4),
  legend.var2.values.rnd = -2,
  legend.var2.pos = "left", 
  legend.var2.title.txt = "Median\nIncome\n(in euros)"
) 
# layout
layoutLayer(title="Population & Wealth in Martinique, 2015", 
            author = "cartography 2.1.3", 
            sources = "Sources: Insee and IGN, 2018", 
            scale = 5, tabtitle = TRUE, frame = FALSE)
# north arrow
north(pos = "topleft")

## ----proptypo, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# Plot the municipalities
plot(st_geometry(mtq), col="#f2efe9", border="#b38e43", bg = "#aad3df", 
     lwd = 0.5)
# Plot symbols with choropleth coloration
propSymbolsTypoLayer(
  x = mtq, 
  var = "POP", 
  inches = 0.5,
  symbols = "square",
  border = "white",
  lwd = .5,
  legend.var.pos = "topright", 
  legend.var.title.txt = "Population",
  var2 = "STATUS",
  legend.var2.values.order = c("Prefecture", "Sub-prefecture", 
                               "Simple municipality"),
  col = carto.pal(pal1 = "multi.pal", n1 = 3),
  legend.var2.pos = c(693000, 1607000), 
  legend.var2.title.txt = "Administrative\nStatus"
) 
# layout
layoutLayer(title="Population Distribution in Martinique", 
            author = "cartography 2.1.3", 
            sources = "Sources: Insee and IGN, 2018", 
            scale = 5, tabtitle = TRUE, frame = FALSE)
# north arrow
north(pos = "topleft")

## ----labelMap, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# plot municipalities
plot(st_geometry(mtq), col = "#e4e9de", border = "darkseagreen4", 
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
  sources = "Sources: Insee and IGN, 2018",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = TRUE, 
  tabtitle = TRUE, 
  theme = "taupe.pal"
) 


## ----linkMap, fig.height=6, fig.width=5, margin=TRUE---------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# path to the csv file embedded in cartography
path_to_csv <- system.file("csv/mob.csv", package="cartography")
# import to a data.frame
mob <- read.csv(path_to_csv)
# select workplaces with administrative status = Prefecture or Sub-prefecture
mob <- mob[mob$sj != "Simple municipality",]
# create an sf object of links
mtq_mob <- getLinkLayer(
  x = mtq, 
  xid = "INSEE_COM", 
  df = mob, 
  dfid = c("i","j")
)
# set figure background color
par(bg="grey25")
# plot municipalities
plot(st_geometry(mtq), col = "grey13", border = "grey25", 
     bg = "grey25", lwd = 0.5)
# plot graduated links
gradLinkTypoLayer(
  x = mtq_mob, 
  xid = c("i", "j"),
  df = mob,
  dfid = c("i","j"),
  var = "fij", 
  breaks = c( 100,  500, 1200, 2500, 4679.0),
  lwd = c(1,4,8,16),
  legend.var.pos = "left",
  legend.var.title.txt = "Nb. of\nCommuters",
  var2 = "sj", 
  col = c("grey85", "red4"),
  legend.var2.title.txt = "Workplace",
  legend.var2.pos = "topright"
) 
# map layout
layoutLayer(title = "Commuting to Prefectures in Martinique", 
            sources = "Sources: Insee and IGN, 2018",  
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, col = "grey25", coltitle = "white",
            tabtitle = TRUE)

## ----isopleth, fig.height=6, fig.width=5, margin=TRUE--------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# plot isopleth map
smoothLayer(
  x = mtq, 
  var = 'POP',
  typefct = "exponential",
  span = 4000,
  beta = 2,
  nclass = 12,
  col = carto.pal(pal1 = 'brown.pal', n1 = 12),
  border = "grey",
  lwd = 0.1, 
  mask = mtq, 
  legend.values.rnd = -3,
  legend.title.txt = "Population\nPotential",
  legend.pos = "topright", 
  add=TRUE
)
# annotation on the map
text(x = 692582, y = 1611478, cex = 0.8, adj = 0, font = 3,  labels = 
       "Distance function:\n- type = exponential\n- beta = 2\n- span = 4 km")
# layout
layoutLayer(title = "Population Distribution in Martinique",
            sources = "Sources: Insee and IGN, 2018",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE, theme = "brown.pal")
# north arrow
north(pos = "topleft")

## ----grid, fig.height=6, fig.width=5, margin=TRUE------------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# Create a grid layer, cell size area match the median municipality area 
mygrid <- getGridLayer(
  x = mtq, 
  cellsize = median(as.numeric(st_area(mtq))), 
  var = "POP",
  type = "hexagonal"
)
# Compute population density in people per km2
mygrid$POPDENS <- 1e6 * mygrid$POP / mygrid$gridarea
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "#deffff")
# Plot the population density
choroLayer(x = mygrid, var = "POPDENS", method = "geom", nclass=5, 
           col = carto.pal(pal1 = "turquoise.pal", n1 = 5), border = "grey80", 
           lwd = 0.5, legend.pos = "topright", add = TRUE,
           legend.title.txt = "Population Density\n(people per km2)") 
layoutLayer(title = "Population Distribution in Martinique", 
            sources = "Sources: Insee and IGN, 2018",
            author = paste0("cartography ", packageVersion("cartography")), 
            frame = FALSE, north = FALSE, tabtitle = TRUE,
            theme = "turquoise.pal")
# north arrow
north(pos = "topleft")

## ----discc, fig.height=5, fig.width=5, margin=TRUE-----------------------
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# Compute the population density (inhab./km2) using sf::st_area()
mtq$POPDENS <- as.numeric(1e6 * mtq$POP / st_area(mtq))
# Get a SpatialLinesDataFrame of countries borders
mtq.contig <- getBorders(mtq)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1", 
     xlim = c(690574, 745940))
# Plot the population density with custom breaks
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
            sources = "Sources: Insee and IGN, 2018",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "grey.pal")
# north arrow
north(pos = "topleft")

## ----sp, fig.height=3.6, fig.width=5, margin=TRUE------------------------
library(sp)
library(cartography)
data("nuts2006")
# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")
# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border = NA, add = TRUE)
# Plot Nuts2 regions
plot(nuts0.spdf, col = "grey60",border = "white", lwd = 0.4, add = TRUE)
# plot the countries population
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
# layout
layoutLayer(title = "Population in Europe, 2008",
            sources = "Data: Eurostat, 2008",
            author =  paste0("cartography ", packageVersion("cartography")),
            scale = 500, frame = TRUE, col = "#688994") 
# north arrow
north("topleft")

