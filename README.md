# `cartography`

***Create and integrate maps into your R workflow.***   


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



Functions starting with `get` **build** R objects.  
Functions ending with `Layer` **plot** cartographic layers.  
Functions starting with `legend` **plot** legends.  


## Installation

```{r}
require(devtools)
devtools::install_github("Groupe-ElementR/cartography")
```

## Demo

```{r}
vignette(topic = "cartography")
```




