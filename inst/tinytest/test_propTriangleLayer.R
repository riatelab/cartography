library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

plot(st_geometry(mtq))
expect_silent(propTrianglesLayer(mtq, var1 = "ACT", var2 = "CHOM", add=TRUE))
plot(st_geometry(mtq))
expect_silent(propTrianglesLayer(mtq, var2 = "ACT", var1 = "CHOM", add=TRUE))
expect_silent(propTrianglesLayer(mtq, var2 = "ACT", var1 = "CHOM", add=FALSE))
