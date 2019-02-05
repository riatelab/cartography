context("Plot Layout Layer")


test_that("layout is plotting", {
  plot(st_geometry(mtq$geom))
  expect_silent(layoutLayer())
  plot(st_geometry(mtq$geom))
  expect_silent(layoutLayer(tabtitle=TRUE, postitle = "right", south = TRUE))
  expect_silent(layoutLayer(tabtitle = TRUE, postitle = "center", north = TRUE, 
                            theme = "blue.pal", extent = mtq))
  expect_silent(layoutLayer(tabtitle = TRUE, postitle = "center", north = TRUE, 
                            theme = "blue.pal", extent = as(mtq, "Spatial")))
  expect_silent(layoutLayer(scale=5, horiz = FALSE, author="    AUTHOR"))
})