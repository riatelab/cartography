library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

expect_silent(typoLayer(mtq, var="STATUS"))
expect_silent(typoLayer(spdf=as(mtq, "Spatial"), var="STATUS"))
mtq$STATUS[1:3] <- NA
expect_silent(typoLayer(x=getPencilLayer(mtq,100), var="STATUS"))
