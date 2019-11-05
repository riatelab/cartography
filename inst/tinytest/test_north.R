library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

fufun <- function(){
  for (i in list("topleft", "top", "topright", "right", "bottomright",
                 "bottom", "bottomleft", "left")){
    north(i, south = FALSE)
  }
}

plot(st_geometry(mtq))
expect_silent(fufun())
expect_silent(north(pos =  c(746368, 1632993), south = TRUE))
expect_silent(north(x=mtq))
