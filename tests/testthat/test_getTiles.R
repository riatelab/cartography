context("Tiles Layer")

test_that("Tiles class and dl", {
  skip_on_cran()
  expect_is(getTiles(x = mtq), "RasterBrick")
  # expect_message(getTiles(x = mtq, verbose=TRUE))
  expect_warning(getTiles(spdf = as(mtq, "Spatial"), zoom = 1))
  expect_is(getTiles(x = as(mtq, "Spatial")), "RasterBrick")
  st_crs(mtq) <- NA
  expect_error(getTiles(x = mtq, zoom = 1))
})

test_that("Tiles Plot", {
  skip_on_cran()
  x <- getTiles(x=mtq)
  expect_silent(tilesLayer(x, add = FALSE))
  expect_silent(tilesLayer(x, add = TRUE))
}) 

