context("Plot Proportional Symbols Choro")

test_that("plots are plotting", {
  plot(st_geometry(mtq))
  expect_silent(propSymbolsChoroLayer(mtq, var="POP", var2 = "MED",  add=TRUE))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsChoroLayer(mtq, var="POP", var2 = "MED", add=TRUE, symbols = "square"))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsChoroLayer(mtq, var="POP", var2 = "MED", add=TRUE, symbols = "bar"))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsChoroLayer(mtq, var="POP", var2 = "MED", add=TRUE, symbols = "square"))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsChoroLayer(spdf = as(mtq, "Spatial"), var="POP", var2 = "MED", add=TRUE))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsChoroLayer(mtq, var="POP", var2 = "MED", fixmax = 100000,add=TRUE))
  mtq$MED[1:2] <- NA
  expect_silent(propSymbolsChoroLayer(mtq, var="POP", var2 = "MED", add=FALSE))
})
