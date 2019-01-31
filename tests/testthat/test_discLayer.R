context("Plot Discontinuity Layer")

mtq.borders <- getBorders(x = mtq)
plot(st_geometry(mtq))

test_that("disc is plotting", {
  expect_silent(discLayer(x = mtq.borders, df = mtq,
                          var = "MED", col="red4", nclass=3,
                          method="equal", threshold = 0.4, sizemin = 0.5,
                          sizemax = 10, type = "abs",legend.values.rnd = 0,
                          legend.title.txt = "Discontinuities\n(absolute difference)",
                          legend.pos = "bottomleft", add=TRUE))
  expect_silent(discLayer(x = mtq.borders, df = mtq,
                          var = "MED", col="red4", nclass=3,
                          method="equal", threshold = 0.4, sizemin = 0.5,
                          sizemax = 10, type = "rel",legend.values.rnd = 0,
                          legend.title.txt = "Discontinuities\n(absolute difference)",
                          legend.pos = "bottomleft", add=TRUE))
  expect_error(discLayer(spdf = mtq.borders))
})



