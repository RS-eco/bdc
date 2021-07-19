#' ---
#' title: "Process Human Footprint data for Bavaria"
#' author: "RS-eco"
#' ---

#' Details about the work are provided in the following papers:
  
#' Oscar Venter, Eric W. Sanderson, Ainhoa Magrach, James R. Allan, Jutta Beher, Kendall R. Jones, 
#' Hugh P. Possingham, William F. Laurance, Peter Wood, Balázs M. Fekete, Marc A. Levy & James E. M. Watson, 2016. 
#' Sixteen years of change in the global terrestrial human footprint and implications for biodiversity conservation. 
#' Nature Communications 7:12558. DOI:10.1038/ncomms12558

#' Venter et al. 2016. Global terrestrial Human Footprint maps for 1993 and 2009. Sci. Data 3:160067. DOI: 10.1038/sdata.2016.67

#' Full data can be downloaded from Data Dryad: http://dx.doi.org/10.5061/dryad.052q5.

#' Summary data has been downloaded from: https://wcshumanfootprint.org/

#' The human footprint map measures the cumulative impact of direct pressures on nature from human activities. 
#' It includes eight inputs:

#'1. the extent of built environments,
#'2. crop land,
#'3. pasture land,
#'4. human population density,
#'5. night-time lights,
#'6. railways,
#'7. roads, and
#'8. navigable waterways. 

# Specify file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/HFP/"

# Download file
#download.file("https://datadryad.org/stash/downloads/download_resource/27531", destfile=paste0(filedir, "doi_10.5061_dryad.052q5__v2.zip"))
#download.file("https://wcshumanfootprint.org/data/HFP2009.zip", destfile=paste0(filedir, "HFP2009.zip"))
#download.file("https://wcshumanfootprint.org/data/HFP1993.zip", destfile=paste0(filedir, "HFP1993.zip"))

files_1993 <- list.files(paste0(filedir, "/Maps"), pattern="19.*\\.tif$", full.names=T)
files_2009 <- list.files(paste0(filedir, "/Maps"), pattern="20.*\\.tif$", full.names=T)
files <- list.files(paste0(filedir, "/Maps"), pattern="R.*\\.tif$", full.names=T)

# Load files
hfp_1993_v3_bav <- raster::stack(c(files_1993[c(1,2,4:8)], files))
hfp_2009_v3_bav <- raster::stack(c(files_2009[c(1,2,4:8)], files))

# Crop by extent of Bavaria
load("data/bavaria.rda")
sp::spTransform(bavaria, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
hfp_1993_v3_bav <- raster::crop(hfp_1993_v3_bav, raster::extent(c(682000, 1071000, 5584000, 5933000)))
hfp_2009_v3_bav <- raster::crop(hfp_2009_v3_bav, raster::extent(c(682000, 1071000, 5584000, 5933000)))

# Re-project data
hfp_1993_v3_bav <- raster::projectRaster(hfp_1993_v3_bav, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
hfp_2009_v3_bav <- raster::projectRaster(hfp_2009_v3_bav, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Mask data
hfp_1993_v3_bav <- raster::mask(hfp_1993_v3_bav, bavaria, snap="out")
hfp_2009_v3_bav <- raster::mask(hfp_2009_v3_bav, bavaria, snap="out")
raster::plot(hfp_2009_v3_bav)
sp::plot(bavaria, add=T)

# Turn into data.frame
hfp_1993_v3_bav <- as.data.frame(raster::rasterToPoints(hfp_1993_v3_bav))
hfp_2009_v3_bav <- as.data.frame(raster::rasterToPoints(hfp_2009_v3_bav))
colnames(hfp_1993_v3_bav)
colnames(hfp_2009_v3_bav)
head(hfp_2009_v3_bav)

# Save to file
save(hfp_1993_v3_bav, file="data/hfp_1993_v3_bav.rda", compress="xz")
save(hfp_2009_v3_bav, file="data/hfp_2009_v3_bav.rda", compress="xz")
