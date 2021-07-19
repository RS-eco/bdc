#' ---
#' title: "Process ISIMIP data for Bavaria"
#' author: "RS-eco"
#' ---

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/ISIMIP2b/EWEMBI/"

# Get daily temperature EWEMBI ISIMIP2b files
tas_files <- list.files(filedir, pattern="^tas_", full.names=T)
tasmax_files <- list.files(filedir, pattern="^tasmax_", full.names=T)
tasmin_files <- list.files(filedir, pattern="^tasmin_", full.names=T)
pr_files <- list.files(filedir, pattern="^pr_", full.names=T)

#remotes::install_github("RS-eco/processNC")
library(processNC)

# Load and crop data by extent of Bavaria
ext_bav <- c(5, 15, 45, 55)
tas_data <- sapply(tas_files, function(x) processNC::cropNC(x, ext_bav, tempfile(pattern="tas_", fileext=".nc")))
tasmax_data <- sapply(tasmax_files, function(x) processNC::cropNC(x, ext_bav, tempfile(pattern="tasmax_", fileext=".nc")))
tasmin_data <- sapply(tasmin_files, function(x) processNC::cropNC(x, ext_bav, tempfile(pattern="tasmin_", fileext=".nc")))
pr_data <- sapply(pr_files, function(x) processNC::cropNC(x, ext_bav, tempfile(pattern="pr_", fileext=".nc")))

load("data/bavaria.rda")
ewembi_bav <- lapply(1:4,function(x){
  dat <- list(tasmin_data, tasmax_data, tas_data, pr_data)[[x]]
  dat <- raster::projectRaster(raster::stack(dat), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  dat <- raster::mask(dat, bavaria)
  dat <- as.data.frame(raster::rasterToPoints(dat))
  colnames(dat) <- c("x", "y", gsub("-", ".", seq(as.Date("1979-01-01"),as.Date("2016-12-31"), by="day")))
  dat$var <- c("tasmin", "tasmax", "tas", "pr")[x]
  return(dat)
})
ewembi_bav <- dplyr::bind_rows(ewembi_bav)
head(colnames(ewembi_bav))
tail(colnames(ewembi_bav))
head(ewembi_bav[,13880:13883])
tail(ewembi_bav[,13880:13883])
library(dplyr)
ewembi_bav %>% filter(var=="tasmin") %>% 
  ggplot2::ggplot() + ggplot2::geom_tile(ggplot2::aes(x=x, y=y, fill=`2016.12.15`)) + ggplot2::coord_map() + 
  ggplot2::geom_polygon(data=bavaria, ggplot2::aes(x=long, y=lat, group=group), fill=NA, colour="black")
#save(ewembi_bav, file="data/ewembi_bav.rda", compress="xz")

library(dplyr); library(tidyr)
ewembi_bav <- ewembi_bav %>% gather(date, value, -c(x,y,var)) %>% spread(var, value) 
head(ewembi_bav)
tail(ewembi_bav)
save(ewembi_bav, file="data/ewembi_bav.rda", compress="xz")

#load("data/tk25_grid.rda")
#tk25_r <- raster::rasterFromXYZ(tk25_grid)
#ewembi_bav_tk25 <- lapply(1:4,function(x){
#  dat <- list(tasmin_data, tasmax_data, tas_data, pr_data)[[x]]
#  dat <- raster::projectRaster(raster::stack(dat), crs=sp::CRS("+init=epsg:31468"))
#  dat <- raster::resample(dat, tk25_r, method="bilinear")
#  dat <- raster::mask(dat, tk25_r)
#  dat <- as.data.frame(raster::rasterToPoints(dat))
#  colnames(dat) <- c("x", "y", gsub("-", ".", seq(as.Date("1979-01-01"),as.Date("2016-12-31"), by="day")))
#  dat$var <- c("tasmin", "tasmax", "tas", "pr")[x]
#  return(dat)
#})
#ewembi_bav_tk25 <- dplyr::bind_rows(ewembi_bav_tk25)
#head(colnames(ewembi_bav_tk25))
#tail(colnames(ewembi_bav_tk25))
#head(ewembi_bav_tk25[,13880:13883])
#tail(ewembi_bav_tk25[,13880:13883])
#save(ewembi_bav_tk25, file="data/ewembi_bav_tk25.rda", compress="xz")
