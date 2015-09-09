
#### Package description
#' @title Cartography Package
#' @name cartography
#' @description 
#' The cartography package proposes thematic mapping functions.
#' It allows various cartographic representation:  
#' \itemize{
#' \item{Proportionnal symbols maps (circles, squares, bars)\cr 
#' \link{propSymbolsLayer}, \link{propSymbolsChoroLayer}, 
#' \link{propSymbolsTypoLayer}, \link{propTrianglesLayer}}
#' \item{Chroropleth maps (main discretization methods are availables)\cr
#' \link{choroLayer}}
#' \item{Typology maps\cr
#' \link{typoLayer}}
#' \item{Flow maps (proportionnal and classified links)\cr
#' \link{getLinkLayer}, \link{propLinkLayer}, \link{gradLinkLayer}}
#' \item{Discontinuities maps (variable size and color of borders)\cr
#' \link{getBorders}, \link{discLayer}}
#' \item{...}
#' }
#' 
#' It also proposes some additional usefull features like:
#' \itemize{
#' \item{Cartographic palettes (palettes adapted to cartographic representation)\cr
#' \link{carto.pal}}
#' \item{Layout (scale, north arrow, title...)\cr
#' \link{layoutLayer}}
#' \item{Labels\cr
#' \link{labelLayer}}
#' \item{Nice legends \cr
#' \link{legendBarsSymbols},
#' \link{legendChoro}, \link{legendCirclesSymbols}, \link{legendGradLines}, 
#' \link{legendPropLines}, \link{legendPropTriangles}, 
#' \link{legendSquaresSymbols}, \link{legendTypo}
#' }
#' \item{Access to cartographic API (via OpenStreetMap package)\cr
#' \link{getTiles}, \link{tilesLayer}
#' }
#' \item{Irregular polygons to regular grid transformation with data handling\cr
#' \link{getGridLayer}, \link{getGridData}}
#' \item{...}
#' }
#' 
#' Functions starting with "get" build R objects.\cr
#' Functions ending with "Layer" plot cartographic layers.\cr
#' Functions starting with "legend" plot legends.\cr
#' 
#' @docType package
NULL

#' @title Color Palettes
#' @name cartography.colors
#' @description List of color gradients adapted to thematic cartography.
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts3 Regions
#' @name nuts3.spdf
#' @description Delineations of EU administrative units (level 3, 2006 version).
#' @format SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be used with the nuts3.df data frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts2 Regions
#' @name nuts2.spdf
#' @description Delineations of EU administrative units (level 2, 2006 version).
#' @format SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be used with the nuts2.df data frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts1 Regions
#' @name nuts1.spdf
#' @description Delineations of EU administrative units (level 1, 2006 version).
#' @format SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be used with the nuts1.df data frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts0 Regions
#' @name nuts0.spdf
#' @description Delineations of EU administrative units (level 0, 2006 version).
#' @format SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be used with the nuts0.df data frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts0 Dataset
#' @name nuts0.df
#' @description This dataset contains some socio-economic data
#' @details This data frame can be used with the SpatialPolygonsDataFrame 
#' nuts0.spdf
#' @field id Unique nuts id (character)
#' @field emp2008 Active population in employment in 2008 (thousands persons) 
#' (numeric)
#' @field act2008 Active population in 2008 (thousands persons) (numeric)
#' @field unemp2008 Active population unemployed in 2008 (thousands persons) 
#' (numeric)
#' @field birth_2008 Number of birth in 2008 (live birth) (numeric)
#' @field death_2008 Number of death in 2008 (death) (numeric)
#' @field gdppps1999 Gross domestic product (Purchasing Power Standards) in 
#' 1999 (million euros) (numeric)
#' @field gdppps2008 Gross domestic product (Purchasing Power Standards) in 
#' 2008 (million euros) (numeric)
#' @field pop1999 Total population in 1999 (inhabitants) (numeric)
#' @field pop2008 Total population in 2008 (inhabitants) (numeric)
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts1 Dataset
#' @name nuts1.df
#' @description This dataset contains some socio-economic data
#' @details This data frame can be used with the SpatialPolygonsDataFrame 
#' nuts1.spdf
#' @field id Unique nuts id (character)
#' @field emp2008 Active population in employment in 2008 (thousands persons) 
#' (numeric)
#' @field act2008 Active population in 2008 (thousands persons) (numeric)
#' @field unemp2008 Active population unemployed in 2008 (thousands persons) 
#' (numeric)
#' @field birth_2008 Number of birth in 2008 (live birth) (numeric)
#' @field death_2008 Number of death in 2008 (death) (numeric)
#' @field gdppps1999 Gross domestic product (Purchasing Power Standards) in 1999 
#' (million euros) (numeric)
#' @field gdppps2008 Gross domestic product (Purchasing Power Standards) in 2008 
#' (million euros) (numeric)
#' @field pop1999 Total population in 1999 (inhabitants) (numeric)
#' @field pop2008 Total population in 2008 (inhabitants) (numeric)
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts2 Dataset
#' @name nuts2.df
#' @description This dataset contains some socio-economic data
#' @details This data frame can be used with the SpatialPolygonsDataFrame 
#' nuts2.spdf
#' @field id Unique nuts id (character)
#' @field emp2008 Active population in employment in 2008 (thousands persons) 
#' (numeric)
#' @field act2008 Active population in 2008 (thousands persons) (numeric)
#' @field unemp2008 Active population unemployed in 2008 (thousands persons) 
#' (numeric)
#' @field birth_2008 Number of birth in 2008 (live birth) (numeric)
#' @field death_2008 Number of death in 2008 (death) (numeric)
#' @field gdppps1999 Gross domestic product (Purchasing Power Standards) in 1999 
#' (million euros) (numeric)
#' @field gdppps2008 Gross domestic product (Purchasing Power Standards) in 2008 
#' (million euros) (numeric)
#' @field pop1999 Total population in 1999 (inhabitants) (numeric)
#' @field pop2008 Total population in 2008 (inhabitants) (numeric)
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts3 Dataset
#' @name nuts3.df
#' @description This dataset contains some socio-economic data
#' @details This data frame can be used with the SpatialPolygonsDataFrame 
#' nuts3.spdf
#' @field id Unique nuts id (character)
#' @field birth_2008 Number of birth in 2008 (live birth) (numeric)
#' @field death_2008 Number of death in 2008 (death) (numeric)
#' @field gdppps1999 Gross domestic product (Purchasing Power Standards) in 1999 
#' (million euros) (numeric)
#' @field gdppps2008 Gross domestic product (Purchasing Power Standards) in 2008 
#' (million euros) (numeric)
#' @field pop1999 Total population in 1999 (inhabitants) (numeric)
#' @field pop2008 Total population in 2008 (inhabitants) (numeric)
#' @source UMS RIATE
#' @docType data
NULL

#' @title Coastline of Europe
#' @name coasts.spdf
#' @description Coastline of Europe.
#' @format SpatialLinesDataFrame.
#' @source UMS RIATE
#' @docType data
NULL

#' @title Frame around Europe
#' @name frame.spdf
#' @description Frame around European countries.
#' @format SpatialPolygonsDataFrame.
#' @source UMS RIATE
#' @docType data
NULL

#' @title Graticule around Europe
#' @name graticule.spdf
#' @description Graticule around Europe.
#' @format SpatialLinesDataFrame.
#' @source UMS RIATE
#' @docType data
NULL


#' @title Countries in the European Area
#' @name countries.spdf
#' @description Countries in the european area.
#' @format SpatialPolygonsDataFrame.
#' @source UMS RIATE
#' @docType data
NULL

#' @title World Background
#' @name world.spdf
#' @description World background.
#' @format SpatialPolygonsDataFrame.
#' @source UMS RIATE
#' @docType data
NULL

#' @title Twin Cities Dataset
#' @name twincities
#' @description This dataset contains the number of international twinning 
#' agreements betwwen cities. Agreements are aggregated at nuts2 level.
#' @details This data frame can be used with the SpatialPolygonsDataFrame 
#' nuts2.spdf
#' @field i nuts2 identifier
#' @field j nuts2 identifier
#' @field fij number of agreements
#' @source Adam Ploszaj - Centre for European Regional and Local Studies EUROREG, 
#' University of Warsaw, Poland. 
#' Primary source: Wikipedia.
#' @docType data
NULL
