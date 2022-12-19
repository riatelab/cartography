library(sf)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
expect_error(getTiles(x = mtq, zoom = 1))

