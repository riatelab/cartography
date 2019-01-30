context("Get Borders")


res <- getBorders(x = mtq)
res2 <- getBorders(spdf = as(mtq, "Spatial"), spdfid = "INSEE_COM")
res3 <- getBorders(x = as(mtq, "Spatial"))

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
