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

plot(st_geometry(mtq))
test_that("legend typos", {
  expect_silent( legendTypo(pos = "bottomleft",
                            col = c("red", "blue"), categ = c('red','blue'), 
                            cex = 0.75,
                            nodata.txt = "no data", frame = TRUE, symbol="box"))
  expect_silent( legendTypo(pos = "nada", 
                            col = c("red", "blue"), categ = c('red','blue')))
})

test_that("legend prop lines", {
  expect_silent(legendPropLines(pos = "n", var = c(10,100), lwd = 0.5, 
                                frame = T))
  expect_silent(legendPropLines(pos = "top", var = c(10,100), lwd = 0.5, 
                                frame = T))
})

test_that("legend grad lines", {
  expect_silent(legendGradLines(pos = "n", breaks = c(1,2,3)))
  expect_silent(legendGradLines(pos = "top", breaks = c(1,2,3), lwd = c(1,2), 
                                frame = T, col = 'red'))
})


plot(st_geometry(mtq))
test_that("legend triangle", {
  var <- runif(10, 0,100)
  var2 <- runif(10, 0,100)
  r <- sqrt(var)*1000
  r2 <- sqrt(var2)*1000
  expect_silent( legendPropTriangles(
    pos = "topright", var.txt = "population 1",
    var2.txt = "population 2", title.txt="Population totale",
    title.cex = 0.8, values.cex = 0.6, cex = 1,
    var = var, var2 = var2, r = r, r2 = r2,
    col="green", col2="yellow", frame=TRUE, values.rnd=2,
    style="e"))
})



plot(st_geometry(mtq))
test_that("other posititons", {
  expect_silent(legendTypo(pos = c(698207.5, 1610389), 
                           col = c("red", "blue"), categ = c('red','blue')))
  expect_silent(legendTypo(pos = "left", 
                           col = c("red", "blue"), categ = c('red','blue')))
  expect_silent(legendTypo(pos = "bottom", 
                           col = c("red", "blue"), categ = c('red','blue')))
  expect_silent(legendTypo(pos = "center", 
                           col = c("red", "blue"), categ = c('red','blue')))
})