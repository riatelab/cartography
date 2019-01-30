context("Get Borders")
library(cartography)
library(sf)
library(sp)
library(methods)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
res <- getBorders(x = mtq)
res2 <- getBorders(spdf = as(mtq, "Spatial"))
res3 <- getBorders(x = as(mtq, "Spatial"))
# 
# class(st_geometry(mtq.borders))[1]
# # Plot polygons
# plot(st_geometry(mtq), border = NA, col = "grey60")
# # Plot borders
# plot(st_geometry(mtq.borders), 
#      col = sample(x = rainbow(nrow(mtq.borders))), 
#      lwd = 3, add = TRUE)



test_that("number of borders", {
  expect_equal(nrow(res), 142)
  expect_equal(nrow(res2), 142)
  expect_equal(nrow(res3), 142)
})

test_that("borders class", {
  expect_is(st_geometry(res), "sfc_MULTILINESTRING")
  expect_is(st_geometry(res2), "sfc_MULTILINESTRING")
  expect_is(st_geometry(res3), "sfc_MULTILINESTRING")
})

# test_that("str_length of factor is length of level", {
#   expect_equal(str_length(factor("a")), 1)
#   expect_equal(str_length(factor("ab")), 2)
#   expect_equal(str_length(factor("abc")), 3)
# })
# 
# test_that("str_length of missing is missing", {
#   expect_equal(str_length(NA), NA_integer_)
#   expect_equal(str_length(c(NA, 1)), c(NA, 1))
#   expect_equal(str_length("NA"), 2)
# })