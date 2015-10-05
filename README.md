# `cartography`
***Create and integrate maps in your R workflow!***   

## **[Demo](https://rawgit.com/Groupe-ElementR/cartography/master/inst/doc/cartography.html)**


## Features  
`cartography` allows various **cartographic representations**: 

* Proportionnal symbols maps (circles, squares, bars)   
`propSymbolsLayer`, `propSymbolsChoroLayer`, `propSymbolsTypoLayer`, `propTrianglesLayer`  

* Chroropleth maps (main discretization methods are availables)  
`choroLayer`  

* Typology maps  
`typoLayer`  

* Flow maps (proportionnal and classified links)   
`getLinkLayer`, `propLinkLayer`, `gradLinkLayer`  

* Discontinuities maps (variable size and color of borders)  
`getBorders`, `discLayer`

* ...

It also proposes some **additional usefull features** like:

* Cartographic palettes (palettes adapted to cartographic representation)  
`carto.pal`  

* Layout (scale, north arrow, title...)  
`layoutLayer`  

* Labels  
`labelLayer`  

* Nice legends   
`legendBarsSymbols`, `legendChoro`, `legendCirclesSymbols`, `legendGradLines`, `legendPropLines`, `legendPropTriangles`, `legendSquaresSymbols`, `legendTypo`  

* Access to cartographic API (via OpenStreetMap package)  
`getTiles`, `tilesLayer`  

* Irregular polygons to regular grid transformation with data handling  
`getGridLayer`, `getGridData`  

* ...


## Principles

`cartography` uses R base graphics. 

Functions starting with `get` **build** R objects.  
Functions ending with `Layer` **plot** cartographic layers.  
Functions starting with `legend` **plot** legends.  


## Installation
### From GitHub
Development version
```{r}
require(devtools)
devtools::install_github("Groupe-ElementR/cartography")
```

### From CRAN
Stable version
```{r}
install.packages("cartography")
```

## Demo
The vignette contains commented scripts on how to build various types of maps with `cartography`

```{r}
vignette(topic = "cartography")
```




