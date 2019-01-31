context("Plot Dot Density Layer")

plot(st_geometry(mtq))
test_that("dots are plotting", {
  expect_silent(dotDensityLayer(x = mtq,  var="POP", pch=20, col = "red4", 
                                legend.frame = FALSE, n = 500))
  expect_silent(dotDensityLayer(spdf = as(mtq, "Spatial"),  var="POP", pch=20, col = "red4", 
                                legend.frame = FALSE))
  expect_silent(dotDensityLayer(x = mtq,  var="POP", pch=20, col = "red4", n = 500, add=F))
})
