#' ---
#' title: "Create Worldclim v2 data for Bavaria"
#' author: "RS-eco"
#' ---

#' ## Load libraries and data

# Load libraries
library(tidyverse); library(sf)

load("data/tk4tel_grid.rda")

# Load shapefile of bavaria
load("data/bavaria.rda")
bavaria_gk <- sp::spTransform(bavaria, sp::CRS("+init=epsg:31468"))

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/Worldclim"

# Get Worldclim v2 data
res <- "30s" #"30s", "2.5m", "5m", "10m"

tmin <- raster::stack(list.files(paste0(filedir, "/wc2.0_", res, '_tmin'), pattern=".tif", full.names=T))
tmax <- raster::stack(list.files(paste0(filedir, "/wc2.0_", res, '_tmax'), pattern=".tif", full.names=T))
tavg <- raster::stack(list.files(paste0(filedir, "/wc2.0_", res, '_tavg'), pattern=".tif", full.names=T))
prec <- raster::stack(list.files(paste0(filedir, "/wc2.0_", res, '_prec'), pattern=".tif", full.names=T))
bio <- raster::stack(list.files(paste0(filedir, "/wc2.0_", res, '_bio'), pattern=".tif", full.names=T))

ext_bav <- c(5, 15, 45, 55)
tmin <- raster::crop(tmin, ext_bav, snap="out")
tmax <- raster::crop(tmax, ext_bav, snap="out")
tavg <- raster::crop(tavg, ext_bav, snap="out")
prec <- raster::crop(prec, ext_bav, snap="out")
bio <- raster::crop(bio, ext_bav, snap="out")

raster::plot(bio[[1]])

#' Worldclim Resolution 5 is 6110 x 9270 m, but we would need 6110 x 5560 m
#' thus we take 0.5 (1km) data and resample data to correct resolution
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
bav_wc <- raster::stack(tmin, tmax, tavg, prec, bio)
(bav_wc_gk <- raster::projectRaster(bav_wc, crs=sp::CRS("+init=epsg:31468")))
wc_bav_tk4tel <- raster::resample(bav_wc_gk, tk4tel_r, method="bilinear")
wc_bav_tk4tel <- raster::mask(wc_bav_tk4tel, tk4tel_r)
raster::plot(wc_bav_tk4tel)
wc_bav_tk4tel <- as.data.frame(raster::rasterToPoints(wc_bav_tk4tel))
(colnames(wc_bav_tk4tel) <- sub("_", "", sub(paste0("_", res, "_"), "", sub("wc2.0","", colnames(wc_bav_tk4tel)))))
head(wc_bav_tk4tel)
assign(paste0("wc2.0_", res, "_bav_tk4tel"), value=wc_bav_tk4tel)
save(list=paste0("wc2.0_", res, "_bav_tk4tel"), file=paste0("data/wc2.0_", res, "_bav_tk4tel.rda"), compress="xz")
