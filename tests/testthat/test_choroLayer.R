context("Plot choropleth")

test_that("choropleth is plotting", {
  expect_silent(choroLayer(mtq, var="MED"))
  expect_silent(choroLayer(spdf=as(mtq, "Spatial"), var="MED"))
  mtq$MED[1:3] <- NA
  expect_silent(choroLayer(x=getPencilLayer(mtq,100), var="MED"))
})
