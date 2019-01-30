context("Plot Scale Bar")

test_that("scale bar plot", {
  plot(st_geometry(mtq))
  expect_silent(barscale(size = 5, pos = c(713709.9, 1596117)))
  expect_silent(barscale(size = NULL, style = "oldschool"))
})