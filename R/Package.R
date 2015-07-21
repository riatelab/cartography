
#### Package description
#' @title Cartography Package
#' @name cartography
#' @description 
#' Cartographic package for R / Package de cartographie pour R
#' 
#' This package proposes various functions usefull in the map creation process.
#' It allows various cartographic representation :
#'   
#' Proportionnal symbols maps (circles, squares, bars)
#' Chroropleth maps (main discretization methods are availables)
#' Flow maps (proportionnal and classified links)
#' Discontinuities maps (variable size and color of borders)
#' 
#' It also proposes some additional usefull features like:
#'   
#'   Cartographic palettes (palettes adapted to the cartographic representation)
#' Layout (scale, nort arrow, title...)
#' Nice legends
#' Access to cartographic API (via OpenStreetMap package)
#' Irregular polygons to regular grid transformation with data handling
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
#' @details This SpatialPolygonsDataFrame can be used with the nuts3.df data.frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts2 Regions
#' @name nuts2.spdf
#' @description Delineations of EU administrative units (level 2, 2006 version).
#' @format SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be used with the nuts2.df data.frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts1 Regions
#' @name nuts1.spdf
#' @description Delineations of EU administrative units (level 1, 2006 version).
#' @format SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be used with the nuts1.df data.frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts0 Regions
#' @name nuts0.spdf
#' @description Delineations of EU administrative units (level 0, 2006 version).
#' @format SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be used with the nuts0.df data.frame 
#' @field id Unique nuts id (character)
#' @field name Official name of the administrative unit
#' @source UMS RIATE
#' @docType data
NULL

#' @title Nuts0 Dataset
#' @name nuts0.df
#' @description This dataset contains some socio-economic data
#' @details This DataFrame can be used with the SpatialPolygonsDataFrame 
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
#' @details This DataFrame can be used with the SpatialPolygonsDataFrame 
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
#' @details This DataFrame can be used with the SpatialPolygonsDataFrame 
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
#' @details This DataFrame can be used with the SpatialPolygonsDataFrame 
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

#' @title Frame Around Europe
#' @name frame.spdf
#' @description Frame around European countries.
#' @format SpatialPolygonsDataFrame.
#' @source UMS RIATE
#' @docType data
NULL

#' @title Graticule Around Europe
#' @name graticule.spdf
#' @description Graticule around Europe.
#' @format SpatialLinesDataFrame.
#' @source UMS RIATE
#' @docType data
NULL


#' @title Countries in the European Area
#' @name countries.spdf
#' @description Countries In The European Area.
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
#' @description This dataset contains the number of twinning agreements betwwen 
#' cities in nuts2 regions.
#' @details This DataFrame can be used with the SpatialPolygonsDataFrame 
#' nuts2.spdf
#' @field i nuts2 identifier
#' @field j nuts2 identifier
#' @field fij number of agreements
#' @source Adam Ploszaj - Centre for European Regional and Local Studies EUROREG, 
#' University of Warsaw, Poland. 
#' Primary source: Wikipedia.
#' @docType data
NULL
