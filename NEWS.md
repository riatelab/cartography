# cartography 2.4.2

## Minor changes
- switch to github cicd infrastructure
- add a comment containing an WKT2 CRS representation to sp objects stored in nuts2006.RData
- increase R minimal version to 3.5.0 (re-serialised RData)



# cartography 2.4.1

## Minor changes
- mute an sp dependant warning in propTrianglesLayer (@rsbivand)
- remove isFALSE to work with older R versions
- bugfix in waffleLayer (@dieghernan)
- add lwgeom to suggests to accomodate sf/lwgeom switching function, SpatialPosition. 





# cartography 2.4.0

## New features
- getTiles(): possibility to use Thunderforest tiles with the apikey arg (@dieghernan)
- getTiles(): new cachedir and forceDownload args to control tiles caching (@arthurgailes & @dieghernan)
- getTiles(): possibility to use a custom tiles server (using a list in type arg)
- barscale(): new parameter unit to plot the scalebar in kilometers, miles or meters (@arthurgailes) 
- add hatchedLayer() and legendHatched() to represent hatched layers (@dieghernan)  
- add getPngLayer() and pngLayer() to import and display png images (@dieghernan)
- add wordcloudLayer() to display words à la wordcloud (@dieghernan)
- add waffleLayer() and legenWaffle() to plot waffle maps (several quantitatives variables on the same map)

## Minor changes
- getTiles() gets a lot of new tiles sources (around 60) (@dieghernan) 
- bug fix in getTiles when x!=POINT & nrow(x) == 1, see #59 (thanks to @drikkes-jue)
- tilesLayer() is enhanced with the possibility to use args from raster::plotRGB()
- getBreaks() is now a full wrap of classIntervals + few other methods. 
- remove unused files martinique.shp






# cartography 2.3.0

## Minor changes
- use tinytest instead of testthat (fewer dependancies)
- use slippymath and code instead of rosm for osm tiles download 
(fewer dependancies)
- use sf instead of sp in dotDensityLayer()
- iter argument is deprecated in dotDensityLayer()
- change in default verbose option in getTiles()
- bug fix in getTiles() when nrow(x) == 1
- bug fix in labelLayer() when nrow(x)==1 and overlap = FALSE
- remove nuts data documentation from index and remove broken links
- merge color palette documentation in one Rd
- merge getBorders() and getOuterBorders() documentation in one Rd

## New features
- add a ghostLayer() function to display an empty layer to give correct xlim 
and ylim to a map (inspired by shadow_spatial() from `ggspatial` of @paleolimbot)
- add an "opentopomap" tiles type in getTiles()

# cartography 2.2.1

## Minor changes
- internaly, use sf instead of sp for hexagonal grid computation.
- bugfix in LegendChoro() no more warning when pos = c(x,y). 
- add param to correct north arrow

# cartography 2.2.0

## Breaking changes

- barscale(): automatic size by default, NULL return no scale bar; 
- layoutlayer(): FALSE, no scale bar is displayed, if set to "auto" an automatic size is used (previously was 0). 0 or NULL will display no scalebar. No default text for sources and author. 



## New features

- barscale(): new position ("bottomright", "bottomleft" or c(x,y))
- legendChoro(): horiz argument to display an horizonthal legend, border to set the 
border color of the boxes
- choroLayer() and propSymbolsChoroLayer(): legend.horiz, legend.var2.horiz, legend.border and legend.var2.border arguments to use new legendChoro arguments.
- layoutLayer(): horiz argument to display sources and author in verticaly in the bottomright corner (horiz = FALSE). posscale argument ("bottomright" or 
"bottomleft") for the scale bar position.
- legend*() and relevant function: new "bottomleftextra" position for the legend. It displays the legend in the bottomleft corner with and extra padding that allows the display of sources and author with layoutLayer() without overlapping. 

## Minor changes

- *typoLayer(): fix incorrect display of typo legends if legend.order is not provided.
- getLinkLayer(): fix error when x and df ids do not perfectly match.
- getTiles(): only suggest OSM basemap sources without API key requirement.
- getTiles(): add source citation recommandation in getTiles()
- small fixes on propTrianglesLayer(), gradLinkTypoLayer(), layoutLayer(), getOuterBorder(), dotDensityLayer(), labelLayer()
- vignette: Introduction/descritpion of the package, sf bases examples and dataset description. 
- README: update
- examples: examples are now based on sf object
- data: addition of a GeoPackage on Martinique municipalities (descritpion on vignette), a csv on work mobilities
- add a test suit
- add a pkgdown website




# cartography 2.1.2 

## Minor changes

- fix in getGridLayer() for type ="hexagon" (sp import)
- addition of a cheat cheat reference in vignette (new vignette with a single link)
- Allow unprojected sf as input for getPencilLayer and prevent sampling error with a warning. 
- vignette correction (deprecated argument)



# cartography 2.1.1 

## Minor changes

- rgeos is back to Imports (was actually needed for some sp <> sf transformation)
- Add getPencilLayer function (color pencil layers)
- small fixes to follow sf API changes (warnings with st_centroids)



# cartography 2.1.0

## Major changes

- sp and sf goes from Depends to Import, this implies the need to use library(sf) or library(sp) before using most of cartography functions. 
- rgeos is not used anymore
- packages sp, sf, graphics, stats, classInt and graphics are no more fully imported in the namespace

## Minor changes

- a lot of typos and mis-spelled words have been corrected
- propSymbols*Layer legends use the lwd argument
- LegendProp* have gained a border and an lwd argument




# cartography 2.0.2

## Minor Changes

- bug fix in labelLayer() when x = SpatialPointsDataFrame.
- bug fix in propSymbols*Layer() when using a POINT layer with add=FALSE.
- bug fix in \*Link\*Layer() (df).
- add R, sf and sp version requirement + sf SystemRequirements
- allow dataframe AND tibbles as input for df argument in relevant functions
- add postitle to set the position of the title in laoutLayer()
- add tabtitle to set the size of the title banner in layoutLayer()



# cartography 2.0.1

## Minor Changes

- bug fix for proportionnal symbols with negative values. 


# cartography 2.0.0

## Breaking changes

- Up to version 1.4.2 cartography was mainly based on sp and and rgeos for its 
spatial data management and geoprocessing operations. These dependencies 
are as much as possible replaced by sf functions since version 2.0.0.
Most functions are kept unchanged except for the addition of an x argument 
used to take sf objects as inputs.

- getBorders, getOuterBorders: an x argument is added in first position, x is added to take sf objects as inputs, if x is used then spdf is not used. spdfid is replaced by id. An sf object (MULTILINESTRING) of borders is returned.
- discLayer: spdf, spdfid1, spdfid2 and dfid are replaced by x (an sf object as outputed by getBorders). 
- getGridLayer: getGridLayer replaces getGridLayer + getGridData, x argument replaces spdf and can take an sf object or an sp object. var is added to enter the name(s) of numeric field(s) to compute into the grid. cellsize refers to the targeted area of the cells. The function returns an sf object
- getLinkLayer: spdf, spdf2, df, spdfid, spdf2id, dfids and dfide arguments are replaced by x, xid, df and dfid. x can be either a Spatial*DataFrame or an sf object. The function returns an sf LINESTRING instead of a SpatialLinesDataFrame.
- propLinkLayer, gradLinkLayer, gradLinkTypoLayer: spdf, df, spdfid, spdfids, spdfide, dfid, dfids and dfide arguments are replaced by x, df, xid and dfid. x must be an sf object (as outputed by getLinkLayer).
- legend* functions have been enhanced to be more customizable. It is now possible to lay legend on specified x & y. 
- propSymbolsLayer: breakval and col2 arguments are defunct, use propSymbolsTypoLayer if needed. 
## Major changes 

- choroLayer, typoLayer, propSymbolsLayer, propSymbolsChoroLayer, propSymbolsTypoLayer, dotDensityLayer, getTiles, getFigDim, labelLayer, smoothLayer: an x argument is added in first position of all these functions. x is added to take sf objects as inputs, if x is used then spdf, df, spdfid, id are not used. 
- labelLayer: possibility to add a colored halo around the labels, option to have non-overlapping labels.
- layoutLayer: the extent argument can take an sf object.
- Each function has an example based on sf object. 
- possibility to use text labels in legendChoro


# cartography 1.4.2 

## Minor changes

- Addition of 2 discretization methods (thanks to @MBunel): 'em' and 'arith'. The "arith" method is based on a arithmetic progression along the variable values. The "em" method is based on subset defined by the mean of a numeric vector.
- Update the list of the available tiles servers (getTiles). This list is reduced and could be extended again depanding on the rosm package evolution.


# cartography 1.4.1

## Major changes  

- Addition of the hexagonal regular grid in getGridLayer.  
- Addition of barscale function to plot a custom scalebar.  
- Addition of north function to plot a custom north (or south) arrow.  

## Minor changes 

- small fix on the msd method (breaks order) in getBreaks function.  
- more precise sources for datasets
- renaming twincities data frame to twincities.df
- small fix for ids in getGridLayer


# cartography  1.4.0

## Major changes  

- Introduce getOuterBorders that allows to build borders (as getBorders) between non-contiguous polygons.
- Addition of the theme parameter in layoutLayer to set col and coltitle according to a cartographic palette.
- discretization function become getBreaks. 
- A new discretisation based on mean and standard deviation is added to getBreaks. 
- Addition of colNA parameter in choro\* and typo\* function to set a color for NA values. 
- k parameter in prop* function is now defunct, using k will stop the function.


## Minor changes 

- As it was a heavy import, SpatialPosition package is back to suggested packages.
- As Jenks and Fisher-Jenks algorithms are based on the same principle and give quite similar results but Fisher-Jenks is much faster, "fisher-jenks" method replaces "jenks" in getBreaks methods.
- In most function, if df is missing then spdf@data is used instead.
- Introduce carto.pal.info() function that gives the list of the cartographic palettes. 
- Addition of two qualitatives color palettes ("pastel.pal"" and "multi.pal").


# cartography 1.3

## Major changes  

- Introduce the smoothLayer function, this function computes smoothed maps of potentials. It uses quickStewart from SpatialPosition package.  
- Most of previously suggested packages are now imported.

## Minor changes   

- Introduce the legend.*.values.order parameter in typoLayer, propSymbolsTypoLayer and gradLinkTypoLayer. This parameter allows to order modalities in the legend and eases the color assignement.   
- Change min value in prop*Layer legends.  
- Suppress zero values in propSymb*Layer.
- getGridLayer is faster.
- getLinkLayer is faster.
- getBorders is faster.




# cartography 1.2 

## Major changes  

- The OpenStreetMap package used to display OpenStreetMap tiles has been replaced by the rosm package. rosm installation is lighter and easier (no Java dependency).  
- The k parameter is deprecated in propSymbolsLayer, propSymbolsTypoLayer and propSymbolsChoroLayer. The inches parameter is to be used instead. This modification allows to compare proportionnal symbols maps.   

## Minor changes 

- Introduce the getFigDim function that helps to find output figure dimension that fit a Spatial object dimension ratio.  
- The discLayer function now returns a (invisible) SpatialLinesDataFrame of discontinuities.  
- NAs are better managed in choroLayer.  
- The graticule.spdf SpatialLinesDataFrame has been expanded.   
- The size of the title box in layoutLayer is fixed to 1.2 lines height.  



# cartography 1.1

## Major changes  

- Correction of the proportionnal symbols legends (square, bar, circle, triangle). The previous version was incorrect (size proportionnal to radii and not surfaces).  
- Add a gradLinkTypoLayer function to plot a layer of colored and graduated links.  

## Minor changes 

- choroLayer: small correction to allow to plot variable named "x" or "y".  
- vignette: correct the name entry, incorrectly named "SpatialPosition"", to "cartography".  
- Update of the figure in the README file.  
- Darker, and more visible, south and north arrows.  
