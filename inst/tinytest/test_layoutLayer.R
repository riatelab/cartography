library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
plot(st_geometry(mtq))
expect_silent(layoutLayer())
plot(st_geometry(mtq))
expect_silent(layoutLayer(tabtitle=TRUE, postitle = "right", south = TRUE))
expect_silent(layoutLayer(tabtitle = TRUE, postitle = "center", north = TRUE, 
                          theme = "blue.pal", extent = mtq))
expect_silent(layoutLayer(tabtitle = TRUE, postitle = "center", north = TRUE, 
                          theme = "blue.pal", extent = as(mtq, "Spatial")))
expect_silent(layoutLayer(scale=5, horiz = FALSE, author="    AUTHOR"))
