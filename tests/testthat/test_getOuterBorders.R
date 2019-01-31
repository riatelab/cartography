context("Get OUter Borders")

x = mtq[c(19,11),]
xsp <- as(x, "Spatial")
xsp$id <- 1:nrow(xsp)


test_that("number of borders", {
  skip_on_cran()
  expect_equal(nrow(getOuterBorders(x)), 2)
})

test_that("check ouput", {
  skip_on_cran()
  expect_is(st_geometry(getOuterBorders(x)), "sfc_MULTILINESTRING")
})

test_that("check inputs", {
  skip_on_cran()
  expect_warning(getOuterBorders(spdf=xsp, spdfid = "id"))
  expect_silent(getOuterBorders(x=xsp))
  spdfid="id"
})
