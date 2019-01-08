# cartography <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version-ago/cartography)](https://cran.r-project.org/package=cartography)
[![](https://cranlogs.r-pkg.org/badges/cartography?color=brightgreen)](https://cran.r-project.org/package=cartography)
[![Build Status](https://travis-ci.org/riatelab/cartography.svg?branch=master)](https://travis-ci.org/riatelab/cartography) 
 [![status](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923/status.svg)](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923)  



## Create and integrate maps in your R workflow! 
This package allows various **cartographic representations** such as 
proportional symbols, choropleth, typology, flows or discontinuities maps. It 
also offers several features enhancing the graphic presentation of maps like 
cartographic palettes, layout elements (scale, north arrow, title...), labels, 
legends or access to some cartographic APIs.

## Cheat Sheet
The [cheat sheet](http://riatelab.github.io/cartography/vignettes/cheatsheet/cartography_cheatsheet.pdf) displays a quick overview of `cartography`'s main features:
[![cartography cheat sheet](https://raw.githubusercontent.com/riatelab/cartography/master/img/cheat_sheet.png)](http://riatelab.github.io/cartography/vignettes/cheatsheet/cartography_cheatsheet.pdf)

## Vignette
The [vignette](https://CRAN.R-project.org/package=cartography/vignettes/cartography.html) 
contains commented scripts on how to build various types of maps with `cartography`:

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



## Blog Posts, Tutorials & Papers


[Giraud, T. and Lambert, N. (2017). “Reproducible Cartography.” In Peterson MP (ed.), _Advances in Cartography and GIScience. ICACI 2017. Lecture Notes in Geoinformation and Cartography._, pp.
173-183. doi: 10.1007/978-3-319-57336-6_13.](https://github.com/riatelab/ReproducibleCartography) (EN)      
[Cartographie reproductible](https://riatelab.github.io/cartographie-reproductible) (FR)  
[New version of the cartography package](https://rgeomatic.hypotheses.org/1205) (EN)    
[SF & cartography V2.0.0](https://rgeomatic.hypotheses.org/1149) (FR)   


## Demo
The following script creates a map of symbols that are proportional to values of a 
first variable and colored to reflect the discretization of a second variable.  


```r
library(cartography)
library(sf) 

# Data Import
# Import a shapefile // this one is distributed within the package
mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
# Compute the share of farmers in the active population
mtq$shareCS1 <- 100 * mtq$C13_CS1/mtq$C13_POP

########## Draft Map
# Plot the communes
plot(st_geometry(mtq))
# Plot symbols with choropleth coloration
propSymbolsChoroLayer(x = mtq, var = "C13_POP", var2 = "shareCS1")

# Add a layout
layoutLayer(title="Farmers in Martinique, 2013")
```

![](https://raw.githubusercontent.com/riatelab/cartography/master/img/map9.png)

```r
########## Final Map
# Set a custom color palette
cols <- carto.pal(pal1 = "wine.pal", n1 = 6)

# set plot margins
opar <- par(mar = c(0,0,1.2,0))

# Plot the communes
plot(st_geometry(mtq), col = "#5F799C", border = "white", 
     bg = "#A6CAE0", lwd = 0.5, add = FALSE)

# Plot symbols with choropleth coloration
propSymbolsChoroLayer(x = mtq, var = "C13_POP", var2 = "shareCS1", col = cols, 
                      inches = 0.4, method = "quantile", border = "grey50", 
                      lwd = 1, legend.var.pos = "topright", legend.var.style = "c",
                      legend.var2.pos = "left", legend.var2.title.txt =  
                        "Share of \nthe population\nworking in\nagriculture (%)", 
                      legend.var.title.txt = "Population aged\n15 and over") 

# Add a layout
layoutLayer(title="Farmers in Martinique, 2013", author = "cartography 2.1.3", 
            sources = "INSEE, 2016", scale = 5, tabtitle = TRUE, frame = FALSE)

# Add a north arrow
north(pos = "topleft")

# restore graphics parameters
par(opar)
```
![](https://raw.githubusercontent.com/riatelab/cartography/master/img/map7.png)


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




