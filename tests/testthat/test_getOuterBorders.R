context("Get OUter Borders")

skip_on_cran()


x = mtq[c(19,11),]
xsp <- as(x, "Spatial")
xsp$id <- 1:nrow(xsp)
res <- getOuterBorders(x)

test_that("number of borders", {
  expect_equal(nrow(res), 2)
})

test_that("check ouput", {
  expect_is(st_geometry(res), "sfc_MULTILINESTRING")
})

test_that("check inputs", {
  expect_warning(getOuterBorders(spdf=xsp, spdfid = "id"))
  expect_silent(getOuterBorders(x=xsp))
  spdfid="id"
})
