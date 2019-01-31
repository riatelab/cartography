context("Get Link Layer")

mob <- read.csv(system.file("csv/mob.csv", package="cartography"))
mob_97209 <- mob[mob$i == 97209, ]
mob.sf <- getLinkLayer(x = mtq, df = mob_97209, dfid = c("i", "j"))

test_that("nb links", {
  expect_equal(nrow(mob.sf), 10 )
})

test_that("inputs", {
  expect_silent(getLinkLayer(x = as(mtq, "Spatial"),df = mob_97209, dfid = c("i", "j")))
  expect_error(getLinkLayer(spdf = as(mtq, "Spatial")))
  expect_error(getLinkLayer(mtq[1:5,], df = mob_97209))
  expect_warning(getLinkLayer(mtq[1:20,], df = mob_97209))
})

