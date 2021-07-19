#' ---
#' title: "Create Worldclim data for Bavaria"
#' author: "RS-eco"
#' ---

# Load libraries
library(tidyverse); library(sf)

load("data/tk4tel_grid.rda")

# Load shapefile of bavaria
load("data/bavaria.rda")
bavaria_gk <- sp::spTransform(bavaria, sp::CRS("+init=epsg:31468"))

filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/"

k <- 4
res <- c("0.5", "2.5", "5", "10")[k]
res_name <- c("30s", "2.5m", "5m", "10m")[k]

# Get current Worldclim data
bav_wc1 <- raster::getData('worldclim', var='tmin', res=res, lon=11.5, lat=49, path=filedir)
bav_wc2 <- raster::getData('worldclim', var='tmax', res=res, lon=11.5, lat=49, path=filedir)
bav_wc3 <- raster::getData('worldclim', var='tmean', res=res, lon=11.5, lat=49, path=filedir)
bav_wc4 <- raster::getData('worldclim', var='prec', res=res, lon=11.5, lat=49, path=filedir)
bav_wc5 <- raster::getData('worldclim', var='bio', res=res, lon=11.5, lat=49, path=filedir)
ext_bav <- c(5, 15, 45, 55)
(bav_wc1 <- raster::crop(bav_wc1, ext_bav, snap="out"))
(bav_wc2 <- raster::crop(bav_wc2, ext_bav, snap="out"))
(bav_wc3 <- raster::crop(bav_wc3, ext_bav, snap="out"))
(bav_wc4 <- raster::crop(bav_wc4, ext_bav, snap="out"))
(bav_wc5 <- raster::crop(bav_wc5, ext_bav, snap="out"))

#' Worldclim Resolution 5 is 6110 x 9270 m, but we would need 6110 x 5560 m
#' thus we take 0.5 (1km) data and resample data to correct resolution
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
bav_wc <- raster::stack(bav_wc1, bav_wc2, bav_wc3, bav_wc4, bav_wc5)
(bav_wc_gk <- raster::projectRaster(bav_wc, crs=sp::CRS("+init=epsg:31468")))
wc_bav_tk4tel <- raster::resample(bav_wc_gk, tk4tel_r, method="bilinear")
wc_bav_tk4tel <- raster::mask(wc_bav_tk4tel, tk4tel_r)

# Turn into data.frame and save to file
wc_bav_tk4tel <- as.data.frame(raster::rasterToPoints(wc_bav_tk4tel))
(colnames(wc_bav_tk4tel) <- gsub("_16", "", colnames(wc_bav_tk4tel)))
head(wc_bav_tk4tel)
assign(paste0("wc1.4_", res_name, "_bav_tk4tel"), value=wc_bav_tk4tel)
save(list=paste0("wc1.4_", res_name, "_bav_tk4tel"), file=paste0("data/wc1.4_", res_name, "_bav_tk4tel.rda"), compress="xz")

# Get future Worldclim data
var <- c("tmin", "tmax", "prec", "bio")
model <- c("GD", "HE", "IP", "MC") #c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
rcp <- c(26, 45, 60, 85)
year <- c(50, 70)
df <- expand.grid(var=var, model=model, rcp=rcp, year=year)

dat <- mapply(function(var, rcp, model, year){
  raster::getData('CMIP5', var=var, rcp=rcp, model=model, year=year, res=0.5, lon=11.5, lat=49, path=filedir)},
  var=df$var, rcp=df$rcp, model=df$model, year=df$year)

