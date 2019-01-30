context("Get Tiles")


test_that("tiles class", {
  skip_on_cran()
  expect_is(getTiles(x = mtq), "RasterBrick")
  expect_message(getTiles(x = mtq, verbose=TRUE))
  expect_warning(getTiles(spdf = as(mtq, "Spatial"), zoom = 1))
  expect_is(getTiles(x = as(mtq, "Spatial")), "RasterBrick")
  st_crs(mtq) <- NA
  expect_error(getTiles(x = mtq, zoom = 1))
  
})
