library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
res <- getBorders(x = mtq)
res2 <- getBorders(spdf = as(mtq, "Spatial"), spdfid = "INSEE_COM")
res3 <- getBorders(x = as(mtq, "Spatial"))

expect_equal(nrow(res), 142)
expect_equal(nrow(res2), 142)
expect_equal(nrow(res3), 142)

expect_true(methods::is(st_geometry(res), "sfc_MULTILINESTRING"))
expect_true(methods::is(st_geometry(res2), "sfc_MULTILINESTRING"))
expect_true(methods::is(st_geometry(res3), "sfc_MULTILINESTRING"))

