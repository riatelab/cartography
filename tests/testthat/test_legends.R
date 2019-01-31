context("Legend Plot")

plot(st_geometry(mtq))

test_that("legend choropleth", {
  expect_silent(legendChoro(pos = "nada", breaks = c(1,2,3,4,10.27,15.2),
                             col = carto.pal(pal1 = "orange.pal",n1 = 5)))
  expect_silent(legendChoro(breaks = c(1,2,3,4,10.27,15.2),
                            col = carto.pal(pal1 = "orange.pal",n1 = 5), 
                            frame = TRUE, symbol = "line"))
}) 

test_that("legend prop symbols", {
  expect_silent(legendCirclesSymbols(pos = "nada", var = c(1,10),inches = 10))
  expect_silent(legendCirclesSymbols(pos = "topright",var = c(1,10),inches = 0.3, 
                                     style="e", 
                                     frame = TRUE))
  expect_silent(legendSquaresSymbols(pos = "nada", var = c(1,10),inches = 10))
  expect_silent(legendSquaresSymbols(pos = "bottomright", var = c(1,10),inches = 0.3, 
                                     style="e", 
                                     frame = TRUE))
  expect_silent(legendBarsSymbols(pos = "nada", var = c(1,10),inches = 10))
  expect_silent(legendBarsSymbols(pos = "bottomleft", var = c(1,10),inches = 0.3, 
                                  style="e", 
                                     frame = TRUE))
}) 
