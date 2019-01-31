context("Plot Proportional Triangles")

test_that("Triangle plots", {
  plot(st_geometry(mtq))
  expect_silent(propTrianglesLayer(mtq, var1 = "ACT", var2 = "CHOM", add=TRUE))
  plot(st_geometry(mtq))
  expect_silent(propTrianglesLayer(mtq, var2 = "ACT", var1 = "CHOM", add=TRUE))
  expect_silent(propTrianglesLayer(mtq, var2 = "ACT", var1 = "CHOM", add=FALSE))
})
