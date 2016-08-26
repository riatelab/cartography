# cartography

[![Version](http://www.r-pkg.org/badges/version/cartography)](https://CRAN.R-project.org/package=cartography/)
![](https://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat) 
[![Travis-CI Build Status](https://travis-ci.org/Groupe-ElementR/cartography.svg?branch=master)](https://travis-ci.org/Groupe-ElementR/cartography)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Groupe-ElementR/cartography?branch=master&svg=true)](https://ci.appveyor.com/project/Groupe-ElementR/cartography)
![](http://cranlogs.r-pkg.org/badges/cartography?color=brightgreen)  
 

## *Create and integrate maps in your R workflow!*

[![Cartographic Mix](http://rgeomatic.hypotheses.org/files/2016/02/cartomix.png "click on the map to see the code")](https://gist.github.com/rCarto/ef52aa4e96a7b628956fbf531143ae68)  

This package allows various **cartographic representations** such as proportional 
symbols, chroropleth, typology, flows or discontinuities. In addition, it also 
proposes some useful features like cartographic palettes, layout (scale,
north arrow, title...), labels, legends or access to cartographic API to ease 
the graphic presentation of maps.  

`cartography` uses R base graphics to map spatial information.  


## Vignette
The [vignette](https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html) 
contains commented scripts on how to build various types of maps with `cartography`
```{r}
vignette(topic = "cartography")
```

## Blog posts, tutorials

[Create and integrate maps in your R workflow with the cartography package](http://rgeomatic.hypotheses.org/842) (EN)  
[Cartographie avec R : le package cartography](http://rgeomatic.hypotheses.org/659) (FR)  
[R pour les cartographes](http://neocarto.hypotheses.org/1859) (FR)    
[Comment faire un carton avec R?](http://rgeomatic.hypotheses.org/category/cartography) (FR - *How to build inset maps*)  
[Tutoriel - Cartographie avec R](http://wukan.ums-riate.fr/r2016/) (FR)  
[Cartographie et traitement de l’information géographique avec R](http://wukan.ums-riate.fr/RUSS/RUSS_2016/) (FR)  
[R pour les cartographes : le package cartography](https://osgeo-fr.github.io/presentations_foss4gfr/2016/J1/R_Cartography_T_Giraud_FOSS4G-fr-2016/FOSS4G-fr-2016.html) (FR)

## Installation
* Development version on GitHub
```{r}
require(devtools)
devtools::install_github("Groupe-ElementR/cartography")
```

* Stable version on [CRAN](https://CRAN.R-project.org/package=cartography/)
```{r}
install.packages("cartography")
```

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



## Community Guidelines

One can contribute to the package through [pull requests](https://github.com/Groupe-ElementR/cartography/pulls) and report issues or ask questions [here](https://github.com/Groupe-ElementR/cartography/issues).




