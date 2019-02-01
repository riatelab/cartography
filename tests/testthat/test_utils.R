context("Utils special cases")

df <- st_set_geometry(mtq, NULL)
test_that("utils sp to sf", {
  expect_silent(choroLayer(spdf = as(mtq, "Spatial"), df = df, var = "MED"))
}) 
  

test_that("no col for choro", {
  expect_silent(choroLayer(x = mtq, breaks = c(1,2,3,200000), var = "MED"))
}) 


test_that("lot of modalities for typo", {
  expect_silent(typoLayer(x = mtq, var = "LIBGEO"))
}) 

test_that("non matching modalities and colors", {
  expect_error(typoLayer(x = mtq, var = "STATUS", col = c('red', 'blue')))
  expect_error(typoLayer(x = mtq, var = "STATUS", col = c('red', 'green', 'blue'),
                         legend.values.order = c("A", "B", "C")))
  
}) 

test_that("no variation in xy for labels", {
  x <- st_as_sf(data.frame(x = rep(0,10), y = rep(0,10), var = rep('a',10)),
                coords = c("x", "y"), crs = 32620)
  plot(st_geometry(x))
  expect_silent(labelLayer(x, txt  = "var", overlap = F))  
}) 

