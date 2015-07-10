
#### Data Description
#' @title Tunisian Delegations Dataset
#' @name TNdeleg
#' @description This dataset contains some socio-economic data on tunisian 
#' delegations in 2004 (population, active population, unemployed population, 
#' housing, households).
#' @usage data(TNdeleg)
#' @format This data.frame has 263 rows and 13 columns.
#' @field del_code delegation code (character)
#' @field del_name delegation name (character)
#' @field pop_m male population (numeric)
#' @field pop_f female population (numeric)
#' @field pop_t total population (numeric)
#' @field act_m active male population of 15 years old and over (numeric)
#' @field act_f active female population of 15 years old and over (numeric)
#' @field act_t total active population of 15 years old and over (numeric)
#' @field unem_m unemployed male population of 15 years old and over (numeric)
#' @field unem_f unemployed female population of 15 years old and over (numeric)
#' @field unem_t unemployed total population of 15 years old and over (numeric)
#' @field house number of household (numeric)
#' @field housing number of housing (numeric)
#' @details This data.frame can be joined to the \code{\link{TNdeleg.spdf}} 
#' SpatialPolygonsDataframe. The \code{del_code} field from TNDeleg matches 
#' the \code{del_code} field from TNdeleg.spdf.
#' @source Institut National de la Statistique (INS). General census of 
#' population and housing 2004 (last update 18-06-2013). Data gathered on 
#' October 2014.
#' @seealso \code{\link{TNdeleg.spdf}}
#' @docType data
NULL

#' @title French Departments Dataset
#' @name FRdep
#' @description This dataset contains some socio-economic data on french
#' metropolitan departments from 1975 to 2011 (population, age structure, 
#' employement, sector of employement).
#' @usage data(FRdep)
#' @format This data.frame has 96 rows and 80 columns.
#' @details This data.frame can be joined to the \code{\link{FRdep.spdf}} 
#' SpatialPolygonsDataframe. The \code{dep_code} field from FRdep matches 
#' the \code{dep_code} field from FRdep.spdf.
#' "\code{****}" in field names refers to the census years (1975, 1982, 1990, 
#' 1999, 2006, 2011).
#' @field dep_code department code (character)
#' @field dep_name department name (character)
#' @field pXXXX_**** Population (numeric). "\code{XXXX}" refers to the 
#' different age groups.
#' \itemize{
#'  \item{p0014: 0-14 years}
#'  \item{p1524: 15-24 years}
#'  \item{p2549: 25-49 years}
#'  \item{p5064: 50-64 years}
#'  \item{p6579: 65-79 years}
#'  \item{p80ol: 80 years and older}
#'  \item{ptota: total population}
#'  }
#' @field empl_**** 25-54 years old active population in employment (numeric).
#' @field unem_**** 25-54 years old unemployed population(numeric).
#' @field farm_**** 25-54 years old population employed in the farming sector 
#' (numeric).
#' @field indu_**** 25-54 years old population employed in the industrial sector 
#' (numeric).
#' @field cons_**** 25-54 years old population employed in the construction 
#' sector 
#' (numeric).
#' @field serv_**** 25-54 years old population employed in the service sector 
#' (numeric).
#' @source Institut national de la statistique et des études économiques 
#' (INSEE). General census of population 1975, 1982, 1990, 1999, 2006, 2011. 
#' Data gathered on November 2014.
#' @seealso \code{\link{FRdep.spdf}}
#' @docType data
#'
NULL

#' @title French Departments Dataset
#' @name FRdep.spdf
#' @description Basemap of the french departments
#' @usage data(FRdep)
#' @format This basemap is a SpatialPolygonsDataFrame.
#' @details This data.frame can be joined to the \code{\link{FRdep.spdf}} 
#' SpatialPolygonsDataframe. The \code{dep_code} field from FRdep matches 
#' the \code{dep_code} field from FRdep.spdf.
#' The projection used is RGF93 / Lambert-93 (EPSG:2154).
#' @field dep_code department code (character)
#' @source Geofla départements 2013, IGN.
#' @seealso \code{\link{FRdep}}
#' @docType data
NULL



#' @title Tunisian Delegations Basemap
#' @name TNdeleg.spdf
#' @description Basemap of the tunisian delegations
#' @usage data(TNdeleg)
#' @format This basemap is a SpatialPolygonsDataFrame.
#' @details This SpatialPolygonsDataFrame can be joined to the 
#' \code{\link{TNdeleg}} data.frame.
#' The \code{del_code} field from TNDeleg matches the \code{del_code} field 
#' from TNdeleg.spdf.
#' The projection used is Lambert azimuthal equal-area projection (EPSG:3035).
#' @field del_code delegation code (character)
#' @source Unknown
#' @seealso \code{\link{TNdeleg}}
#' @docType data
NULL
