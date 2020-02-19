# cartography <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version/cartography)](https://cran.r-project.org/package=cartography)
[![](https://cranlogs.r-pkg.org/badges/cartography?color=brightgreen)](https://cran.r-project.org/package=cartography)
[![Build Status](https://travis-ci.org/riatelab/cartography.svg?branch=master)](https://travis-ci.org/riatelab/cartography)
[![codecov](https://codecov.io/gh/riatelab/cartography/branch/master/graph/badge.svg)](https://codecov.io/gh/riatelab/cartography)
[![status](https://tinyverse.netlify.com/badge/cartography)](https://CRAN.R-project.org/package=cartography)
[![status](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923/status.svg)](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923)
[![minimal R version](https://img.shields.io/badge/R-%E2%89%A5%203.3.0-blue)](https://cran.r-project.org/)
![GitHub top language](https://img.shields.io/github/languages/top/riatelab/cartography)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/riatelab/cartography)
![CRAN/METACRAN](https://img.shields.io/cran/l/cartography)

[![GitHub forks](https://img.shields.io/github/forks/riatelab/cartography?style=social)](https://github.com/riatelab/cartography/network/members)
[![GitHub stars](https://img.shields.io/github/stars/riatelab/cartography?style=social)](https://github.com/riatelab/cartography/stargazers)



## Create and integrate maps in your R workflow! 
This package helps to design **cartographic representations** such as proportional symbols, choropleth, typology, flows or discontinuities maps. It also offers several features that improve the graphic presentation of maps, for instance, map palettes, layout elements (scale, north arrow, title...), labels or legends.

## Cheat Sheet
The [cheat sheet](http://riatelab.github.io/cartography/vignettes/cheatsheet/cartography_cheatsheet.pdf) displays a quick overview of `cartography`'s main features.

<a href="http://riatelab.github.io/cartography/vignettes/cheatsheet/cartography_cheatsheet.pdf"><img src="https://raw.githubusercontent.com/riatelab/cartography/master/img/cheat_sheet.png" alt="cartography cheat sheet" width="300"/></a>


## Vignette
The [vignette](https://CRAN.R-project.org/package=cartography/vignettes/cartography.html) 
contains commented scripts on how to build various types of maps with `cartography`:  

[![](https://raw.githubusercontent.com/riatelab/cartography/master/img/vignettes.png)](https://CRAN.R-project.org/package=cartography/vignettes/cartography.html)

## Other Resources 

* [Giraud T. (2019). Thematic Maps with `cartography`. useR! 2019. Toulouse, France.](https://github.com/rCarto/user2019) (EN)  
* [Giraud T., Lambert N. (2017). Reproducible Cartography. In: Peterson M. (eds) Advances in Cartography and GIScience. ICACI 2017. Lecture Notes in Geoinformation and Cartography. Springer, Cham](https://github.com/riatelab/ReproducibleCartography) (EN)      
* [Blog posts](https://rgeomatic.hypotheses.org/category/cartography) (FR / EN)  


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



