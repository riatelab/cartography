library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
plot(st_geometry(mtq))


expect_silent(legendChoro(pos = "nada", breaks = c(1,2,3,4,10.27,15.2),
                          col = carto.pal(pal1 = "orange.pal",n1 = 5)))
expect_silent(legendChoro(pos = "topright", breaks = c(1,2,3,4,10.27,15.2),
                          col = carto.pal(pal1 = "orange.pal",n1 = 5), 
                          horiz = TRUE, border = NA, nodata = TRUE, 
                          frame = TRUE, nodata.txt = "extralongstringcharacterstuff"))
expect_silent(legendChoro(breaks = c(1,2,3,4,10.27,15.2),
                          col = carto.pal(pal1 = "orange.pal",n1 = 5), 
                          frame = TRUE, symbol = "line"))
expect_silent(legendChoro(pos = "nadada", breaks = c(1,2,3,4,10.27,15.2),
                          col = carto.pal(pal1 = "orange.pal",n1 = 5), 
                          horiz = TRUE, nodata = FALSE))
expect_silent(legendChoro(pos = "bottomright", breaks = c(1,2,3,4,10.27,15.2),
                          col = carto.pal(pal1 = "orange.pal",n1 = 5), 
                          horiz = TRUE, nodata = FALSE))
expect_silent(legendChoro(pos = c(698207.5, 1610389), 
                          breaks = c(1,2,3,4,10.27,15.2),
                          col = carto.pal(pal1 = "orange.pal",n1 = 5), 
                          horiz = TRUE))

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

plot(st_geometry(mtq))
expect_silent( legendTypo(pos = "bottomleft",
                          col = c("red", "blue"), categ = c('red','blue'), 
                          cex = 0.75,
                          nodata.txt = "no data", frame = TRUE, symbol="box"))
expect_silent( legendTypo(pos = "nada", 
                          col = c("red", "blue"), categ = c('red','blue')))

expect_silent(legendPropLines(pos = "n", var = c(10,100), lwd = 0.5, 
                              frame = T))
expect_silent(legendPropLines(pos = "top", var = c(10,100), lwd = 0.5, 
                              frame = T))
expect_silent(legendGradLines(pos = "n", breaks = c(1,2,3)))
expect_silent(legendGradLines(pos = "top", breaks = c(1,2,3), lwd = c(1,2), 
                              frame = T, col = 'red'))


plot(st_geometry(mtq))
var <- runif(10, 0,100)
var2 <- runif(10, 0,100)
r <- sqrt(var)*500
r2 <- sqrt(var2)*500
expect_silent( legendPropTriangles(
  pos = "topright", var.txt = "population 1",
  var2.txt = "population 2", title.txt="Population totale",
  title.cex = 0.8, values.cex = 0.6, cex = 1,
  var = var, var2 = var2, r = r, r2 = r2,
  col="green", col2="yellow", frame=TRUE, values.rnd=2,
  style="e"))
positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", 
               "right", "top", "bottom", "middle")

for (i in positions){
  expect_silent( legendPropTriangles(
    pos = i, var.txt = "population 1",
    var2.txt = "population 2", title.txt="Pop",
    title.cex = 0.8, values.cex = 0.6, cex = .5,
    var = var2, var2 = var, r = r, r2 = r2,
    col="green", col2="yellow", frame=TRUE, values.rnd=2,
    style="e"))
}





plot(st_geometry(mtq))
expect_silent(legendTypo(pos = c(698207.5, 1610389), 
                         col = c("red", "blue"), categ = c('red','blue')))
expect_silent(legendTypo(pos = "left", 
                         col = c("red", "blue"), categ = c('red','blue')))
expect_silent(legendTypo(pos = "bottom", 
                         col = c("red", "blue"), categ = c('red','blue')))
expect_silent(legendTypo(pos = "center", 
                         col = c("red", "blue"), categ = c('red','blue')))
expect_silent(legendTypo(pos = "bottomleftextra", 
                         col = c("red", "blue"), categ = c('red','blue')))
