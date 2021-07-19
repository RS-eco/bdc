#' ## Imperviousness

#' see: https://land.copernicus.eu/pan-european/high-resolution-layers/imperviousness/status-maps

#' Imperviousness data is available for the reference years 2006, 2009, 2012 and 2015, and contains two types of products:
  
#' 1. Status layers

#' The percentage of sealed area is mapped for each status layer for any of the 4 reference years 
#' (e.g. degree of Imperviousness 2012). The status layers are available in the original 20m spatial resolution, 
#' and as aggregated 100m products.

#' 2. Change layers

#' Two types of change products are available for each of the 3-year periods between the 4 reference years 
#' (2006-2009, 2009-2012, 2012-2015), and in addition, 
#' for the period 2006-2012 (that is in line with the 6-year period between two Corine Land Cover products):
  
#' a) A simple layer mapping the percentage of sealing increase or decrease for those pixels that show real sealing 
#' change in the period covered. This product is available in 20m and 100m pixel size.

#' b) A classified change product that maps the most relevant categories of sealing change 
#' (unchanged no sealing, new cover, loss of cover, unchanged sealed, increased sealing, decreased sealing). 
#' This product is available in 20m pixel size only.

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/Imperviousness/"

# Load files

imp_20m_2006 <- raster::stack(paste0(filedir, ".tif"))