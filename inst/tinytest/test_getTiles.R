library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
mtq1 <- mtq[1,]
st_geometry(mtq1) <- st_centroid(st_geometry(mtq1))
home <- length(unclass(packageVersion("cartography"))[[1]]) == 4
if(home){
  expect_true(methods::is(getTiles(x=mtq, crop = TRUE), "RasterBrick"))
  expect_true(methods::is(getTiles(x=mtq1, crop = TRUE), "RasterBrick"))
  suppressMessages(expect_warning(getTiles(x = mtq, verbose=TRUE)))
  expect_warning(getTiles(spdf = as(mtq, "Spatial"), zoom = 1))
  expect_true(methods::is(getTiles(x = as(mtq, "Spatial")), "RasterBrick"))
  x <- getTiles(x = mtq)
  st_crs(mtq) <- NA
  expect_error(getTiles(x = mtq, zoom = 1))
  expect_silent(tilesLayer(x, add = FALSE))
  expect_silent(tilesLayer(x, add = TRUE))
}
