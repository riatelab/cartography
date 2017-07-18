# cartography

[![Version](http://www.r-pkg.org/badges/version/cartography)](https://CRAN.R-project.org/package=cartography/)
[![Travis-CI Build Status](https://travis-ci.org/riatelab/cartography.svg?branch=master)](https://travis-ci.org/riatelab/cartography)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/riatelab/cartography?branch=master&svg=true)](https://ci.appveyor.com/project/Groupe-ElementR/cartography)
![](http://cranlogs.r-pkg.org/badges/cartography?color=brightgreen)
[![status](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923/status.svg)](http://joss.theoj.org/papers/0c2d51fc23efb8e1f87d764da8414923)  
 

## *Create and integrate maps in your R workflow!*

[![Cartographic Mix](http://rgeomatic.hypotheses.org/files/2016/02/cartomix.png "click on the map to see the code")](https://gist.github.com/rCarto/ef52aa4e96a7b628956fbf531143ae68)  

This package allows various **cartographic representations** such as proportional 
symbols, chroropleth, typology, flows or discontinuities maps. It also offers 
several features enhancing the graphic presentation of maps: cartographic palettes, 
layout elements (scale, north arrow, title...), labels, legends or access to 
some cartographic APIs.


`cartography` uses R base graphics to map spatial information.  

-----------

**Up to version 1.4.2 `cartography` was mainly based on `sp` and `rgeos` for its spatial data management and geoprocessing operations. These dependancies are as much as possible replaced by [`sf`](https://github.com/r-spatial/sf) functions since version 2.0.0.**    

**Most functions are kept unchanged except for the addition of an `x` argument used to take `sf` objects as inputs.**  

**See the [NEWS](https://raw.githubusercontent.com/riatelab/cartography/master/NEWS) file for the full list of changes.**   

**For now (2017-06-30) `cartography` v2.0.0 is hosted on GitHub, but will soon make it to the CRAN.** 

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
# Load data
data(nuts2006)
# set margins
opar <- par(mar = c(0,0,1.2,0))

# Compute the compound annual growth rate
nuts2.df$cagr <- (((nuts2.df$pop2008 / nuts2.df$pop1999)^(1/9)) - 1) * 100

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")
# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
# Plot Nuts2 regions
plot(nuts2.spdf, col = "grey60",border = "white", lwd=0.4, add=TRUE)

# Set a custom color palette
cols <- carto.pal(pal1 = "blue.pal", n1 = 2, pal2 = "red.pal", n2 = 4)

# Plot symbols with choropleth coloration
propSymbolsChoroLayer(spdf = nuts2.spdf, 
                      df = nuts2.df, 
                      var = "pop2008", #  field in df to plot the symbols sizes
                      inches = 0.1, # set the symbols sizes
                      var2 = "cagr", #  field in df to plot the colors
                      col = cols, # symbols colors
                      breaks = c(-2.43,-1,0,0.5,1,2,3.1), # breaks
                      border = "grey50",  # border colors of the symbols
                      lwd = 0.75, # symbols width
                      legend.var.pos = "topright", # legend position
                      legend.var.values.rnd = -3, # legend value 
                      legend.var.title.txt = "Total Population", # size legend title
                      legend.var.style = "e", # legend type
                      legend.var2.pos = "right", # legend position
                      legend.var2.title.txt = "Compound Annual\nGrowth Rate") # legend title

# layout
layoutLayer(title = "Demographic trends, 1999-2008", coltitle = "black",
            sources = "Eurostat, 2011", scale = NULL,
            author = "cartography", frame ="", col = NA)
par(opar)
```
![](http://rgeomatic.hypotheses.org/files/2015/10/propchoro.png)


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




