context("Get Grid Layer")

test_that("correct input and functions", {
  expect_error(getGridLayer(x = mtq, cellsize = 3e7, type = "YOLO", var = "POP"))
  expect_error(getGridData())
  expect_warning(getGridLayer(spdf = as(mtq, "Spatial"), cellsize = 4e+08, type = "regular", var = "POP"))
  
})

test_that("Grid class", {
  expect_is(st_geometry(getGridLayer(x = mtq, cellsize = 4e+08, type = "hexagonal", var = "POP")), "sfc_MULTIPOLYGON")
  expect_is(st_geometry(getGridLayer(x = as(mtq, "Spatial"), cellsize = 4e+08, type = "regular", var = "POP")), "sfc_MULTIPOLYGON")
})
