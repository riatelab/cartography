library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

expect_error(getGridLayer(x = mtq, cellsize = 3e7, type = "YOLO", var = "POP"))
expect_error(getGridData())
expect_error(getGridLayer(spdf = as(mtq, "Spatial"), cellsize = 4e+08, type = "regular", var = "POP"))
expect_true(methods::is(st_geometry(
  getGridLayer(
    x = mtq,
    cellsize = 4e+08,
    type = "hexagonal",
    var = "POP"
  )
), "sfc_MULTIPOLYGON"))
expect_true(methods::is(st_geometry(
  getGridLayer(
    x = as(mtq, "Spatial"),
    cellsize = 4e+08,
    type = "regular",
    var = "POP"
  )
), "sfc_MULTIPOLYGON"))
