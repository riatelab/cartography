library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

a <- getPencilLayer(mtq,100)
b <- getPencilLayer(mtq,100, lefthanded = FALSE)


expect_true(methods::is(st_geometry(a), "sfc_MULTILINESTRING"))
expect_true(methods::is(st_geometry(b), "sfc_MULTILINESTRING"))
expect_true(methods::is(st_geometry(getPencilLayer(mtq,10)), 
                        "sfc_MULTILINESTRING"))
expect_error(getPencilLayer(mtq,100, buffer = 1000000))  
mtq$MED[1:3] <- NA
expect_silent(choroLayer(x=getPencilLayer(mtq,100), var="MED"))
