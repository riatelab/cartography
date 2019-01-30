context("Plot choropleth")



a <- getPencilLayer(mtq,100)
b <- getPencilLayer(mtq,100, lefthanded = FALSE)


test_that("choropleth is plotting", {
  expect_is(st_geometry(a), "sfc_MULTILINESTRING")
  expect_is(st_geometry(b), "sfc_MULTILINESTRING")
  expect_is(st_geometry(getPencilLayer(mtq,10)), "sfc_MULTILINESTRING")
  expect_error(getPencilLayer(mtq,100, buffer = 1000000))  
  expect_silent(choroLayer(spdf=as(mtq, "Spatial"), var="MED"))
  mtq$MED[1:3] <- NA
  expect_silent(choroLayer(x=getPencilLayer(mtq,100), var="MED"))
})
