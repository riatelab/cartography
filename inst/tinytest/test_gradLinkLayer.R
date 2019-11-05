library(sf)
library(sp)
mtq <-
  st_read(system.file("gpkg/mtq.gpkg", package = "cartography"), quiet = TRUE)

mob <- read.csv(system.file("csv/mob.csv", package = "cartography"))
mob.sf <-
  getLinkLayer(x = mtq,
               df = mob[mob$j == 97209, ],
               dfid = c("i", "j"))



plot(st_geometry(mtq))
expect_silent(
  gradLinkLayer(
    x = mob.sf,
    df = mob,
    legend.pos = "topright",
    var = "fij",
    breaks = c(109, 500, 1000, 2000, 4679),
    lwd = c(1, 2, 4, 10),
    col = "#92000090",
    add = TRUE
  )
)
expect_error(gradLinkLayer(spdf = mtq))
expect_error(
  gradLinkLayer(
    x = mob.sf,
    df = mob,
    legend.pos = "topright",
    var = "fij",
    breaks = c(109, 500, 1000, 2000, 4679),
    lwd = c(1, 2, 4),
    col = "#92000090",
    add = TRUE
  )
)
