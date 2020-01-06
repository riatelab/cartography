library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
x = mtq[c(19,11),]
xsp <- as(x, "Spatial")
xsp$id <- 1:nrow(xsp)

home <- length(unclass(packageVersion("cartography"))[[1]]) == 4
if(home){
  expect_equal(nrow(getOuterBorders(x)), 2)
#   expect_true(methods::is(st_geometry(getOuterBorders(x)), "sfc_MULTILINESTRING"))
#   expect_warning(getOuterBorders(spdf = xsp, spdfid = "id"))
#   expect_silent(getOuterBorders(x = xsp))
}