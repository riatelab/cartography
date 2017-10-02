# cartography

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/cartography)](https://cran.r-project.org/package=cartography)
[![Travis-CI Build Status](https://travis-ci.org/riatelab/cartography.svg?branch=master)](https://travis-ci.org/riatelab/cartography)
[![Build status](https://ci.appveyor.com/api/projects/status/9kjyos8jxicc02xs?svg=true)](https://ci.appveyor.com/project/rCarto/cartography)
![](http://cranlogs.r-pkg.org/badges/cartography?color=brightgreen)
[![status](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923/status.svg)](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923)  
 

## *Create and integrate maps in your R workflow!*

[![Cartographic Mix](https://raw.githubusercontent.com/riatelab/cartography/master/img/map8.png "click on the map to see the code")](https://gist.github.com/rCarto/ef52aa4e96a7b628956fbf531143ae68)  

This package allows various **cartographic representations** such as proportional 
symbols, chroropleth, typology, flows or discontinuities maps. It also offers 
several features enhancing the graphic presentation of maps: cartographic palettes, 
layout elements (scale, north arrow, title...), labels, legends or access to 
some cartographic APIs.


`cartography` uses R base graphics to map spatial information.  

-----------

**Up to version 1.4.2 `cartography` was mainly based on `sp` and `rgeos` for its spatial data management and geoprocessing operations. These dependencies have been as much as possible replaced by [`sf`](https://github.com/r-spatial/sf) functions since version 2.0.0.**    

**Most functions are kept unchanged except for the addition of an `x` argument used to take `sf` objects as inputs.**  

**See the [NEWS](https://raw.githubusercontent.com/riatelab/cartography/master/NEWS) file for the full list of changes and see [sf README](https://github.com/r-spatial/sf) in case of installation problems with sf.**

-----------



## Vignette
The [vignette](https://CRAN.R-project.org/package=cartography/vignettes/cartography.html) 
contains commented scripts on how to build various types of maps with `cartography`

<table>
<tbody>
<tr>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map1.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map2.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map3.png" /></td>
</tr>
<tr>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map4.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map5.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map6.png" /></td>
</tr>
</tbody>
</table>


```{r}
vignette(topic = "cartography")
```



## Blog Posts, Tutorials & Papers


[Giraud, T. and Lambert, N. (2017). “Reproducible Cartography.” In Peterson MP (ed.), _Advances in Cartography and GIScience. ICACI 2017. Lecture Notes in Geoinformation and Cartography._, pp.
173-183. doi: 10.1007/978-3-319-57336-6_13.](https://github.com/riatelab/ReproducibleCartography)  (EN)    



[New version of the cartography package](https://rgeomatic.hypotheses.org/1205) (EN)     
[Demo codes in the R graph Gallery](http://www.r-graph-gallery.com/portfolio/maps/) (EN)    
[Create and integrate maps in your R workflow with the cartography package](http://rgeomatic.hypotheses.org/842) (EN)  
[SF & cartography V2.0.0](https://rgeomatic.hypotheses.org/1149) (FR)  
[De superbes cartes thématiques...](http://rgeomatic.hypotheses.org/1086) (FR)  
[Le package cartography a un an](http://rgeomatic.hypotheses.org/1016) (FR)    
[Cartographie avec R : le package cartography](http://rgeomatic.hypotheses.org/659) (FR)  
[R pour les cartographes](http://neocarto.hypotheses.org/1859) (FR)    
[Comment faire un carton avec R?](http://rgeomatic.hypotheses.org/category/cartography) (FR - *How to build inset maps*)  
[Tutoriel - Cartographie avec R](http://wukan.ums-riate.fr/r2016/) (FR)  
[Cartographie et traitement de l’information géographique avec R](http://wukan.ums-riate.fr/RUSS/RUSS_2016/) (FR)  
[R pour les cartographes : le package cartography](https://osgeo-fr.github.io/presentations_foss4gfr/2016/J1/R_Cartography_T_Giraud_FOSS4G-fr-2016/FOSS4G-fr-2016.html) (FR)


## Demo

* You can access the code used to create the cartographic mix [here](https://gist.github.com/rCarto/ef52aa4e96a7b628956fbf531143ae68).  

* The following script creates a map of symbols that are proportional to values of a 
first variable and colored to reflect the discretization of a second variable.  

```r
library(cartography)

# Import a shapefile // this one is distributed within the package
mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))

# Share of farmers in the active population
mtq$shareCS1 <- 100 * mtq$C13_CS1/mtq$C13_POP

# Set a custom color palette
cols <- carto.pal(pal1 = "wine.pal", n1 = 6)

# set plot margins
opar <- par(mar = c(0,0,1.2,0))

# Plot the communes
plot(st_geometry(mtq), col = "#5F799C", border = "white", 
     bg = "#A6CAE0", lwd = 0.5, add = FALSE)

# Plot symbols with choropleth coloration
propSymbolsChoroLayer(x = mtq, # sf object 
                      var = "C13_POP", # field used to plot the symbols sizes
                      var2 = "shareCS1", #  field used to plot the colors
                      col = cols, # symbols colors
                      inches = 0.4, # radius of the largest circle
                      method = "quantile", # discretization method (?getBreaks)
                      border = "grey50", # color of circle borders
                      lwd = 1, # width of the circle borders
                      legend.var.pos = "topright", # position of the first legend
                      legend.var2.pos = "left", # position of the second legend
                      legend.var2.title.txt =  
                        "Share of \nthe population\nworking in\nagriculture (%)", 
                      legend.var.title.txt = "Population aged\n15 and over",
                      legend.var.style = "c") # legend style

# Add a layout
layoutLayer(title="Farmers in Martinique, 2013", # title of the map
            scale = 5, # size of the scale bar
            north = TRUE, # north arrow
            col = "white",
            coltitle = "black",
            author = "cartography 2.0.0",  
            sources = "INSEE, 2016",
            frame = TRUE)

# restore graphics parameters
par(opar)
```
![](https://raw.githubusercontent.com/riatelab/cartography/master/img/map7.png)


## Installation
* Development version on GitHub
```{r}
require(devtools)
install_github("riatelab/cartography")
```

* Stable version on [CRAN](https://CRAN.R-project.org/package=cartography/)
```{r}
install.packages("cartography")
```



## Alternatives Packages
* [tmap](https://github.com/mtennekes/tmap)    
* [ggplot2](https://github.com/tidyverse/ggplot2)     
* [ggmap](https://github.com/dkahle/ggmap)    
* [rworldmap](https://github.com/AndySouth/rworldmap/)


## Community Guidelines

One can contribute to the package through [pull requests](https://github.com/riatelab/cartography/pulls) and report issues or ask questions [here](https://github.com/riatelab/cartography/issues).




