# cartography

[![Version](http://www.r-pkg.org/badges/version/cartography)](https://CRAN.R-project.org/package=cartography/)
![](http://cranlogs.r-pkg.org/badges/cartography?color=brightgreen)
[![Travis-CI Build Status](https://travis-ci.org/Groupe-ElementR/cartography.svg?branch=master)](https://travis-ci.org/Groupe-ElementR/cartography)

## *Create and integrate maps in your R workflow!*

![Cartographic Mix](http://rgeomatic.hypotheses.org/files/2016/02/cartomix.png)  

This package allows various **cartographic representations** such as proportional 
symbols, chroropleth, typology, flows or discontinuities. In addition, it also 
proposes some useful features like cartographic palettes, layout (scale,
north arrow, title...), labels, legends or access to cartographic API to ease 
the graphic presentation of maps.  

`cartography` uses R base graphics to map spatial information.  


## Demo

This script creates a map of symbols that are proportional to values of a 
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
                      legend.var.pos = "topright", # size legend position
                      legend.var.values.rnd = -3, # size legend value roundinf
                      legend.var.title.txt = "Total Population", # size legend title
                      legend.var.style = "e", # size legend type
                      legend.var2.pos = "right", # color legend position
                      legend.var2.title.txt = "Compound Annual\nGrowth Rate") # legend title

# layout
layoutLayer(title = "Demographic trends, 1999-2008", coltitle = "black",
            sources = "Eurostat, 2011", scale = NULL,
            author = "cartography", frame ="", col = NA)
par(opar)
```
![Proportional Choropleth](http://rgeomatic.hypotheses.org/files/2015/10/propchoro.png)


## Vignette
The [vignette](https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html) 
contains commented scripts on how to build various types of maps with `cartography`
```{r}
vignette(topic = "cartography")
```


## Installation
* Development version on GitHub
```{r}
require(devtools)
devtools::install_github("Groupe-ElementR/cartography")
```

* Stable version on [CRAN](https://cran.r-project.org/web/packages/cartography/)
```{r}
install.packages("cartography")
```






