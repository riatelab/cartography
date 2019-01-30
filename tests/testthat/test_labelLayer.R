context("Plot Labels")

test_that("labels are plotting", {
  plot(st_geometry(mtq))
  expect_silent(labelLayer(mtq, txt ="LIBGEO"))
  plot(st_geometry(mtq))
  expect_silent(labelLayer(mtq, txt ="LIBGEO", overlap = FALSE, halo = TRUE))
  plot(st_geometry(mtq))
  expect_silent(labelLayer(spdf = as(mtq, "Spatial"), txt ="LIBGEO"))
  plot(st_geometry(mtq))
  expect_silent(labelLayer(x = as(mtq, "Spatial"), txt ="LIBGEO"))
  plot(st_geometry(mtq))
  expect_silent(labelLayer(mtq, txt ="LIBGEO", overlap = FALSE, show.lines = TRUE))
  
})