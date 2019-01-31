context("Plot Typology")

test_that("typo layer is plotting", {
  expect_silent(typoLayer(mtq, var="STATUS"))
  expect_silent(typoLayer(spdf=as(mtq, "Spatial"), var="STATUS"))
  mtq$STATUS[1:3] <- NA
  expect_silent(typoLayer(x=getPencilLayer(mtq,100), var="STATUS"))
})
