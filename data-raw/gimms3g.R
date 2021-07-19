# Download GIMMS3g Data and process for Bavaria

library(ggmap2)

# Download data
#downloadNDVI(startyear=1981, endyear=2013, version="v0", path="/media/matt/Data/Documents/Wissenschaft/Data/GIMMS")
#downloadNDVI(startyear=1981, endyear=2015, version="v1", path="/media/matt/Data/Documents/Wissenschaft/Data/GIMMS")

# Read data
gimms3g_v0_bav <- ndvi3g(extent = c(8, 14, 47, 51), version="v0", startyear=1981, endyear=2013, 
                          path="/media/matt/Data/Documents/Wissenschaft/Data/GIMMS")
flag_v0_bav <- ndvi3g(extent = c(8, 14, 47, 51), version="v0", startyear=1981, endyear=2013, output="flag",
                         path="/media/matt/Data/Documents/Wissenschaft/Data/GIMMS")

#' ## The meaning of the FLAG:

#' FLAG = 7 (missing data)
#' FLAG = 6 (NDVI retrieved from average seasonal profile, possibly snow)
#' FLAG = 5 (NDVI retrieved from average seasonal profile)
#' FLAG = 4 (NDVI retrieved from spline interpolation, possibly snow)
#' FLAG = 3 (NDVI retrieved from spline interpolation)
#' FLAG = 2 (Good value)
#' FLAG = 1 (Good value)

gimms3g_v1_bav <- ndvi3g(extent = c(8, 14, 47, 51), version="v1", startyear=1981, endyear=2015, 
                         path="/media/matt/Data/Documents/Wissenschaft/Data/GIMMS")

# Crop by extent of bavaria
load("data/bavaria.rda")
gimms3g_v0_bav <- raster::mask(gimms3g_v0_bav, bavaria)
flag_v0_bav <- raster::mask(flag_v0_bav, bavaria)
gimms3g_v1_bav <- raster::mask(gimms3g_v1_bav, bavaria)

# Turn into data.frame
gimms3g_v0_bav <- as.data.frame(raster::rasterToPoints(gimms3g_v0_bav))
colnames(gimms3g_v0_bav) <- sub("X", "", colnames(gimms3g_v0_bav))
gimms3g_v0_bav <- tidyr::gather(gimms3g_v0_bav, key="date", value="ndvi", -c(x,y))

flag_v0_bav <- as.data.frame(raster::rasterToPoints(flag_v0_bav))
colnames(flag_v0_bav) <- sub("X", "", colnames(gimms3g_v0_bav))
flag_v0_bav <- tidyr::gather(flag_v0_bav, key="date", value="flag", -c(x,y))
gimms3g_v0_bav <- dplyr::left_join(gimms3g_v0_bav, flag_v0_bav); rm(flag_v0_bav)

gimms3g_v1_bav <- as.data.frame(raster::rasterToPoints(gimms3g_v1_bav))
colnames(gimms3g_v1_bav) <- sub("X", "", colnames(gimms3g_v1_bav))
gimms3g_v1_bav <- tidyr::gather(gimms3g_v1_bav, key="date", value="ndvi", -c(x,y))
head(gimms3g_v1_bav)

# Save to file
save(gimms3g_v0_bav, file="data/gimms3g_v0_bav.rda", compress="xz")
save(gimms3g_v1_bav, file="data/gimms3g_v1_bav.rda", compress="xz")
