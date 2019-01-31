context("Plot Proportional Symbols Typo")

test_that("plots are plotting", {
  plot(st_geometry(mtq))
  expect_silent(propSymbolsTypoLayer(mtq, var="POP", var2 = "STATUS",  add=TRUE))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsTypoLayer(mtq, var="POP", var2 = "STATUS", add=TRUE, symbols = "square"))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsTypoLayer(mtq, var="POP", var2 = "STATUS", add=TRUE, symbols = "bar"))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsTypoLayer(mtq, var="POP", var2 = "STATUS", add=TRUE, symbols = "square"))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsTypoLayer(spdf = as(mtq, "Spatial"), var="POP", var2 = "STATUS", add=TRUE))
  plot(st_geometry(mtq))
  expect_silent(propSymbolsTypoLayer(mtq, var="POP", var2 = "STATUS", fixmax = 100000,add=TRUE))
  mtq$STATUS[1:2] <- NA
  expect_silent(propSymbolsTypoLayer(mtq, var="POP", var2 = "STATUS", add=FALSE))
})
