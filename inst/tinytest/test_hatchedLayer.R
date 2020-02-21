
library(sf)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)


#Errors
expect_error(hatchedLayer(mtq[0,]))
expect_error(hatchedLayer(mtq,pattern = "FF" ))
mtqcent=sf::st_centroid(sf::st_geometry(mtq))
expect_error(hatchedLayer(mtqcent))
mtqline=sf::st_cast(sf::st_geometry(mtq),"POLYGON")
mtqline=sf::st_cast(mtqline,"LINESTRING")
expect_error(hatchedLayer(mtqline))
expect_error(hatchedLayer(mtq, mode="ff"))
expect_error(hatchedLayer(mtq, "text", txt = "Y", add =TRUE))

#pars
mtqpol=st_cast(st_geometry(mtq),"POLYGON")
expect_silent(hatchedLayer(mtqpol, col="green"))
expect_silent(hatchedLayer(mtq, "dot", pch = 23))
expect_silent(hatchedLayer(mtq, "circle", pch = 23, cellsize=10000))
expect_silent(hatchedLayer(mtq, "dot", pch = 23))
expect_silent(hatchedLayer(mtq, "text", txt = "Y"))
expect_silent(hatchedLayer(mtq, "diamond", density = 0.5))
expect_silent(hatchedLayer(mtq, "grid", lwd = 1.5))
expect_silent(hatchedLayer(mtq, "hexagon", col = "blue"))
expect_silent(hatchedLayer(mtq, "horizontal", lty = 5))
expect_silent(hatchedLayer(mtq, "vertical"))
expect_silent(hatchedLayer(mtq, "left2right"))
expect_silent(hatchedLayer(mtq, "right2left"))
expect_silent(hatchedLayer(mtq, "zigzag"))
expect_silent(hatchedLayer(mtq, "circle"))
expect_silent(hatchedLayer(mtq, "circle", cellsize=10000))


expect_true(st_geometry_type(hatchedLayer(mtq,"dot" , mode="legend"))=="MULTIPOINT")
expect_true(st_geometry_type(hatchedLayer(mtq,"hexagon" , mode="legend"))=="MULTILINESTRING")
expect_true(st_geometry_type(hatchedLayer(mtq,"circle" , mode="legend"))=="MULTILINESTRING")
expect_true(st_geometry_type(hatchedLayer(mtq,"zigzag" , mode="legend"))=="MULTILINESTRING")
expect_true(st_geometry_type(hatchedLayer(mtq,"diamond" , mode="legend"))=="MULTILINESTRING")


