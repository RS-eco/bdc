#' ## Download and process global forest watch data for Bavaria

##########

#' Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. 
#' Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. 
#' “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. 
#' Data available on-line from: http://earthenginepartners.appspot.com/science-2013-global-forest. 

##########

#' Tree canopy cover for year 2000 (treecover2000)
#' Tree cover in the year 2000, defined as canopy closure for all vegetation taller than 5m in height. 
#' Encoded as a percentage per output grid cell, in the range 0–100.

#' Global forest cover gain 2000–2012 (gain)
#' Forest gain during the period 2000–2012, defined as the inverse of loss, or a non-forest to forest change entirely within the study period. 
#' Encoded as either 1 (gain) or 0 (no gain).

#' Year of gross forest cover loss event (lossyear)
#' Forest loss during the period 2000–2018, defined as a stand-replacement disturbance, or a change from a forest to non-forest state. 
#' Encoded as either 0 (no loss) or else a value in the range 1–17, representing loss detected primarily in the year 2001–2018, respectively.

#' Data mask (datamask)
#' Three values representing areas of no data (0), mapped land surface (1), and permanent water bodies (2).

#' Circa year 2000 Landsat 7 cloud-free image composite (first)
#' Reference multispectral imagery from the first available year, typically 2000. 
#' If no cloud-free observations were available for year 2000, imagery was taken from the closest year with cloud-free data, within the range 1999–2012.

#' Circa year 2018 Landsat cloud-free image composite (last)
#' Reference multispectral imagery from the last available year, typically 2018. 
#' If no cloud-free observations were available for year 2018, imagery was taken from the closest year with cloud-free data, within the range 2010–2015. 

# Set file directory
#filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/GFC-2018/"
filedir <- "D:/GFC-2018"
if(!dir.exists(filedir)){dir.create(filedir)}

# Specify needed data combinations
df <- data.frame(N=c(50, 50, 60, 60), E=c("000", "010", "000", "010"))

variables <- c("treecover2000", "lossyear", "datamask", "first", "last", "gain")

# Download data for 50N, 000E; 50N, 010E, 60N, 000E; 60N, 010E
lapply(1:nrow(df), function(x){
  lapply(variables, function(var){
    if(!file.exists(paste0(filedir, "/Hansen_GFC-2018-v1.6_", var, "_", df$N[x], "N_", df$E[x], "E.tif"))){
      download.file(paste0("https://storage.googleapis.com/earthenginepartners-hansen/GFC-2018-v1.6/Hansen_GFC-2018-v1.6_", 
                           var, "_", df$N[x], "N_", df$E[x], "E.tif"), 
                    destfile=paste0(filedir, "/Hansen_GFC-2018-v1.6_", var, "_", df$N[x], "N_", df$E[x], "E.tif"))
      
    }
  })
})
###
# Automatic download does not seem to work on bigBoost, as files cannot be loaded afterwards!!!
###

# Load shapefile of bavaria
load("data/bavaria.rda")

# Read data
gfc_bav <- lapply(1:nrow(df), function(x){
  files <- list.files(filedir, pattern=paste0(df$N[x], "N_", df$E[x], "E.tif"), full.names=T)
  gfc <- raster::stack(files)
  # Crop by extent of bavaria
  raster::mask(raster::crop(gfc, bavaria), bavaria, snap="out")
}); rm(gfc)
gfc_bav <- do.call(raster::merge, gfc_bav)
raster::plot(gfc_bav)
gc()
raster::writeRaster(gfc_bav, filename="data/gfc_bav.tif", format="GTiff")

gfc_bav <- raster::stack("data/gfc_bav.tif")

# Turn into data.frame
gfc_bav <- as.data.frame(raster::rasterToPoints(gfc_bav))
colnames(gfc_bav) <- c("x", "y", "datamask", "first-band1", "first-band2", "first-band3","first-band4", "gain", "last-band1", "last-band2",
                       "last-band3", "last-band4", "lossyear", "treecover2000")
head(gfc_bav)

# Subset data
gfc_bav <- dplyr::select(gfc_bav, c(x,y,datamask, gain, lossyear, treecover2000))

#library(ggplot2)
#ggplot() + geom_raster(data=tdm_fnf_bav, aes(x=x,y=y,fill=fnf)) + coord_sf()

# Save to file
save(gfc_bav, file="data/gfc_bav.rda", compress="xz")
