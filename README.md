# cartography <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version-ago/cartography)](https://cran.r-project.org/package=cartography)
[![](https://cranlogs.r-pkg.org/badges/cartography?color=brightgreen)](https://cran.r-project.org/package=cartography)
[![Build Status](https://travis-ci.org/riatelab/cartography.svg?branch=master)](https://travis-ci.org/riatelab/cartography)
[![codecov](https://codecov.io/gh/riatelab/cartography/branch/test/graph/badge.svg)](https://codecov.io/gh/riatelab/cartography)
 [![status](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923/status.svg)](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923)  



## Create and integrate maps in your R workflow! 
This package allows various **cartographic representations** such as 
proportional symbols, choropleth, typology, flows or discontinuities maps. It 
also offers several features enhancing the graphic presentation of maps like 
cartographic palettes, layout elements (scale, north arrow, title...), labels, 
legends or access to some cartographic APIs.

## Cheat Sheet
The [cheat sheet](http://riatelab.github.io/cartography/vignettes/cheatsheet/cartography_cheatsheet.pdf) displays a quick overview of `cartography`'s main features.

<a href="http://riatelab.github.io/cartography/vignettes/cheatsheet/cartography_cheatsheet.pdf"><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/cheat_sheet.png" alt="cartography cheat sheet" width="300"/></a>


## Vignette
The [vignette](https://CRAN.R-project.org/package=cartography/vignettes/cartography.html) 
contains commented scripts on how to build various types of maps with `cartography`:

<table>
<tbody>
<tr>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map03.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map02.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map06.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map04.png" /></td>
</tr>
<tr>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map01.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map08.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map09.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map05.png" /></td>
</tr>
<tr>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map07.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map10.png" /></td>
<td><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/map11.png" /></td>
</tr>
</tbody>
</table>



## Blog Posts, Tutorials & Papers


[Giraud, T. and Lambert, N. (2017). “Reproducible Cartography.” In Peterson MP (ed.), _Advances in Cartography and GIScience. ICACI 2017. Lecture Notes in Geoinformation and Cartography._, pp.
173-183. doi: 10.1007/978-3-319-57336-6_13.](https://github.com/riatelab/ReproducibleCartography) (EN)      
[Cartographie reproductible](https://riatelab.github.io/cartographie-reproductible) (FR)  
[New version of the cartography package](https://rgeomatic.hypotheses.org/1205) (EN)    
[SF & cartography V2.0.0](https://rgeomatic.hypotheses.org/1149) (FR)   


## Demo
The following script creates a map of symbols that are proportional to values of a 
first variable and colored to reflect the classification of a second variable.  


```r
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_file <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_file, quiet = TRUE)

########## Draft Map
# Plot the municipalities
plot(st_geometry(mtq))
# Plot symbols with choropleth coloration (population & median income)
propSymbolsChoroLayer(x = mtq, var = "POP", var2 = "MED")
# Add a layout
title(main = "Population & Wealth in Martinique, 2015", 
      sub = "Sources: Insee and IGN - 2018")
```

![](https://raw.githubusercontent.com/riatelab/cartography/master/img/readme_raw.png)

```r
########## Final Map
# Set figure margins
opar <- par(mar = c(0,0,1.2,0))
# Plot the municipalities
plot(st_geometry(mtq), col="darkseagreen3", border="darkseagreen4",  
     bg = "lightblue1", lwd = 0.5)
# Plot symbols with choropleth coloration
propSymbolsChoroLayer(x = mtq, var = "POP", inches = 0.4, border = "grey50",
                      lwd = 1, legend.var.pos = "topright", 
                      legend.var.title.txt = "Population",
                      var2 = "MED", method = "equal", nclass = 4, 
                      col = carto.pal(pal1 = "sand.pal", n1 = 4),
                      legend.var2.values.rnd = -2,  legend.var2.pos = "left", 
                      legend.var2.title.txt = "Median Income\n(in euros)") 
# Plot a layout
layoutLayer(title="Population & Wealth in Martinique, 2015", 
            author = "cartography 2.1.3", 
            sources = "Sources: Insee and IGN - 2018", 
            scale = 5, tabtitle = TRUE, frame = FALSE)
# Plot a north arrow
north(pos = "topleft")
# restore graphics parameters
par(opar)
```
![](https://raw.githubusercontent.com/riatelab/cartography/master/img/readme_final.png)


## Installation
* Development version on GitHub
```{r}
require(remotes)
install_github("riatelab/cartography")
```

* Stable version on [CRAN](https://CRAN.R-project.org/package=cartography/)
```{r}
install.packages("cartography")
```



## Alternatives Packages
* [tmap](https://github.com/mtennekes/tmap)    
* [ggplot2](https://github.com/tidyverse/ggplot2) + [ggspatial](https://github.com/paleolimbot/ggspatial)     
  


## Community Guidelines

One can contribute to the package through [pull requests](https://github.com/riatelab/cartography/pulls) and report issues or ask questions [here](https://github.com/riatelab/cartography/issues).




