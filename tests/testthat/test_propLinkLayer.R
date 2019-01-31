context("Plot Proportional Link Layer")


mob <- read.csv(system.file("csv/mob.csv", package="cartography"))
mob.sf <- getLinkLayer(x = mtq, df = mob[mob$j==97209,], dfid = c("i", "j"))


test_that("plots are plotting", {
  plot(st_geometry(mtq), col = "grey60",border = "grey20")
  propLinkLayer(x = mob.sf, df = mob,
                maxlwd = 10,
                legend.pos = "topright",
                var = "fij",
                col = "#92000090", add = TRUE)
})

test_that("input", {
  expect_error(propLinkLayer(spdf = mtq))

})
