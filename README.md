# cartography <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version/cartography)](https://cran.r-project.org/package=cartography)
[![R-CMD-check](https://github.com/riatelab/cartography/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/riatelab/cartography/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/riatelab/cartography/branch/master/graph/badge.svg)](https://app.codecov.io/gh/riatelab/cartography)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.00054/status.svg)](https://doi.org/10.21105/joss.00054)



## Consider [`mapsf`](https://riatelab.github.io/mapsf/)

There are no plans for new features or enhancements in `cartography`. 
Basic maintenance and support will continue indefinitely. 
Existing projects that use `cartography` can safely continue to use `cartography`. 
The [`mapsf` R package](https://riatelab.github.io/mapsf/) is the successor of `cartography` and it is friendlier, lighter and more robust. 
See [`mapsf` vignette](https://riatelab.github.io/mapsf/articles/mapsf.html) or [this blog post](https://rgeomatic.hypotheses.org/2212) to migrate from `cartography` to `mapsf`.




### Create and integrate maps in your R workflow! 
This package helps to design **cartographic representations** such as proportional symbols, choropleth, typology, flows or discontinuities maps. It also offers several features that improve the graphic presentation of maps, for instance, map palettes, layout elements (scale, north arrow, title...), labels or legends.


### Demo
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


### Installation
* Development version on GitHub
```{r}
remotes::install_github("riatelab/cartography")
```

* Stable version on [CRAN](https://CRAN.R-project.org/package=cartography/)
```{r}
install.packages("cartography")
```


### Alternatives

* [mapsf](https://github.com/riatelab/mapsf) (successor of `cartography`)
* [tmap](https://github.com/r-tmap/tmap)    
* [ggplot2](https://github.com/tidyverse/ggplot2) + [ggspatial](https://github.com/paleolimbot/ggspatial)     


### Community Guidelines

One can contribute to the package through [pull requests](https://github.com/riatelab/cartography/pulls) and report issues or ask questions [here](https://github.com/riatelab/cartography/issues).

<br><br><br>
<small>
To cite package `cartography` in publications use one of these:  

* [Giraud, T. and Lambert, N. (2016). cartography: Create and Integrate Maps in your R Workflow. JOSS, 1(4). doi: 10.21105/joss.00054.](https://doi.org/10.21105/joss.00054)
* [Giraud, T. and Lambert, N. (2017). “Reproducible Cartography.” In Peterson M. (ed.), _Advances in Cartography and GIScience. ICACI 2017. Lecture Notes in Geoinformation and Cartography._, pp. 173-183. doi: 10.1007/978-3-319-57336-6_13.](https://github.com/riatelab/ReproducibleCartography)  
</small>
