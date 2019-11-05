library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

expect_silent(display.carto.all())

expect_silent(display.carto.pal("turquoise.pal"))

expect_equal(carto.pal(pal1 = "wine.pal", n1 = 3, 
                       pal2 = "blue.pal", n2 = 3, 
                       middle = TRUE, transparency = TRUE), c("#8B1713", "#D35C61b4", 
                                                              "#F0ABAE69", "#F6F6F61e",
                                                              "#96D1EA69", 
                                                              "#4D95BAb4", "#135D89"))
expect_equal(carto.pal(pal1 = "wine.pal", n1 = 3, transparency = TRUE), 
             c("#F0ABAE69","#D35C61b4","#8B1713"))



