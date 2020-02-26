library(sf)
library(sp)
library(sf)

mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
ghostLayer(mtq)

expect_silent(legendWaffle(categ = c("type1", "type2"), 
                           col = c("tomato1", "turquoise")))
expect_silent(legendWaffle(categ = c("type1", "type2"), 
                           col = c("tomato1", "turquoise", cell.size = 750)))
expect_silent(legendWaffle(categ = c("type1", "type2"), frame = TRUE,
                           col = c("tomato1", "turquoise", cell.size = 750)))
expect_silent(legendWaffle(categ = c("type1", "type2"), pos = "haut",
                           col = c("tomato1", "turquoise")))
