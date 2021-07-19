#' ---
#' title: "Extract ISIMIP bioclim and land-use data for Bavaria"
#' author: "RS-eco"
#' ---

# Load packages
library(rISIMIP)
library(tidyverse)

# Load tk25 grid
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)

#List data files
d <- data(package="rISIMIP")

#Load bioclim data
bioclim_files <- d$results[,"Item"][grep(x=d$results[,"Item"], pattern="bioclim")]
bioclim_ref <- lapply(bioclim_files, function(x) raster::rasterFromXYZ(get(data(list=x))))
bioclim_ref <- lapply(bioclim_ref, function(x){
  names(x) <- paste0("bio", 1:19)
  raster::projection(x) <- sp::CRS("+init=epsg:4326")
  return(x)
})
names(bioclim_ref) <- basename(bioclim_files)

# Resample data and subset by extent of bavaria
bioclim_ref <- lapply(bioclim_ref, function(x) raster::projectRaster(x, crs=sp::CRS("+init=epsg:31468")))
bioclim_ref <- lapply(bioclim_ref, function(x) raster::resample(x, tk4tel_r, method="ngb"))
bioclim_ref <- lapply(bioclim_ref, function(x) raster::mask(x, tk4tel_r))
isimip_bio_bav_tk4tel <- lapply(1:length(bioclim_ref), function(x){
  data <- as.data.frame(raster::rasterToPoints(bioclim_ref[[x]]))
  data$Year <-  strsplit(basename(bioclim_files[x]), split="_")[[1]][4]
  data$Year[data$Year == "landonly"] <- 1995
  data$Model <- strsplit(basename(bioclim_files[x]), split="_")[[1]][2]
  data$RCP <- strsplit(basename(bioclim_files[x]), split="_")[[1]][3]
  data$RCP[data$RCP == 1995] <- NA
  return(data)
})
isimip_bio_bav_tk4tel <- dplyr::bind_rows(isimip_bio_bav_tk4tel)
save(isimip_bio_bav_tk4tel, file="data/isimip_bio_bav_tk4tel.rda", compress="xz")

# Load land-use data
landuse_files <- d$results[,"Item"][grep(x=d$results[,"Item"], pattern="landuse-totals_")][2:17]
landuse_ref <- lapply(landuse_files, function(x) raster::rasterFromXYZ(get(data(list=x))))
landuse_ref <- lapply(landuse_ref, function(x){
  raster::projection(x) <- sp::CRS("+init=epsg:4326")
  return(x)
})
names(landuse_ref) <- basename(landuse_files)

# Resample data and subset by extent of bavaria
landuse_ref <- lapply(landuse_ref, function(x) raster::projectRaster(x, crs=sp::CRS("+init=epsg:31468")))
landuse_ref <- lapply(landuse_ref, function(x) raster::resample(x, tk4tel_r, method="ngb"))
landuse_ref <- lapply(landuse_ref, function(x) raster::mask(x, tk4tel_r))
isimip_lu_bav_tk4tel <- lapply(1:length(landuse_ref), function(x){
  data <- as.data.frame(raster::rasterToPoints(landuse_ref[[x]]))
  data$Year <-  strsplit(strsplit(basename(landuse_files[x]), split="_")[[1]][4], split="[.]")[[1]][1]
  data$Model <- strsplit(basename(landuse_files[x]), split="_")[[1]][2]
  data$RCP <- strsplit(basename(landuse_files[x]), split="_")[[1]][1]
  return(data)
})
isimip_lu_bav_tk4tel <- do.call(plyr::rbind.fill, isimip_lu_bav_tk4tel)
save(isimip_lu_bav_tk4tel, file="data/isimip_lu_bav_tk4tel.rda", compress="xz")
