library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet=TRUE)
target <- mtq[30,]
expect_silent(ghostLayer(target, bg = "lightblue"))
expect_silent(ghostLayer(as(target, "Spatial")))
