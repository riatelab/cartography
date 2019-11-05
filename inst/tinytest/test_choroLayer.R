library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
expect_silent(choroLayer(mtq, var="MED", method = "q6"))
expect_silent(choroLayer(spdf=as(mtq, "Spatial"), var="MED"))
mtq$MED[1:3] <- NA
expect_silent(choroLayer(x=getPencilLayer(mtq,100), var="MED"))
