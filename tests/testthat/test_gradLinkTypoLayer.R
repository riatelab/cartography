context("Plot Gradual Link Layer")


mob <- read.csv(system.file("csv/mob.csv", package="cartography"))
mob.sf <- getLinkLayer(x = mtq, df = mob[mob$j %in% c(97209, 97213),], 
                       dfid = c("i", "j"))


test_that("plots are plotting", {
  plot(st_geometry(mtq))
  expect_silent(gradLinkTypoLayer(x = mob.sf, df = mob,
                                  var = "fij", 
                                  breaks = c(109,500,1000,2000,4679), 
                                  lwd = c(1,2,4,10),
                                  var2='j', add = TRUE))
  plot(st_geometry(mtq))
  mob$sj[1:50] <- NA
  expect_silent(gradLinkTypoLayer(x = mob.sf, df = mob,
                                  var = "fij", 
                                  breaks = c(109,500,1000,2000,4679), 
                                  lwd = c(1,2,4,10),
                                  var2='sj', add = TRUE))
})

test_that("input", {
  expect_error(gradLinkTypoLayer(spdf = mtq))
  expect_error(gradLinkTypoLayer(x = mob.sf, df = mob,
                                 var = "fij", 
                                 breaks = c(109,500,1000,2000,4679), 
                                 lwd = c(1,2,4),
                                 var2='j', add = TRUE))
})
