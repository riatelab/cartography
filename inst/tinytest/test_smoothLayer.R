library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

expect_silent(smoothLayer(x = mtq, var = 'POP',
                          span = 4000, beta = 2,
                          mask = mtq, border = NA,
                          col = carto.pal(pal1 = 'wine.pal', n1 = 8),
                          legend.title.txt = "Population\nPotential",
                          legend.pos = "topright", legend.values.rnd = 0))
expect_silent(smoothLayer(spdf = as(mtq,"Spatial"), var = 'POP',
                          span = 4000, beta = 2,
                          mask = mtq, border = NA,
                          col = carto.pal(pal1 = 'wine.pal', n1 = 8),
                          legend.title.txt = "Population\nPotential",
                          legend.pos = "topright", legend.values.rnd = 0))

