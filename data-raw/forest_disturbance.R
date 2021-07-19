#' ---
#' title: "Forest disturbance of Bavaria"
#' author: "RS-eco"
#' ---

filedir <- "/home/matt/t6p/group_hof/@BayKliF/data/"

file <- paste0(filedir, "Seidl_forest_disturbance.zip")

tmp_dir <- tempdir()
dat <- unzip(file, exdir=tmp_dir)
dat <- raster::stack(dat)
dat
names(dat)

#plot(dat[[2]])

# Load shapefile of Bavaria
library(bdc)
data(bavaria)
bavaria <- sf::st_transform(bavaria, raster::crs(dat))

# Change resolution to 300 m
dat_1 <- raster::aggregate(dat[[1]], fact=10, fun=min, na.rm=T)
dat_2 <- raster::aggregate(dat[[2:3]], fact=10, fun=sum, na.rm=T)/100
dat <- raster::stack(dat_1, dat_2); rm(dat_1, dat_2); gc()

# Crop data by extent of Bavaria
dat <- raster::mask(raster::crop(dat, bavaria), bavaria)
#plot(dat)
gc()

# Turn data into data.frame
forest_disturbance_bav_300m <- as.data.frame(raster::rasterToPoints(dat))
rm(dat); gc(); file.remove(list.files(tmp_dir, full.names=T, recursive=T))
colnames(forest_disturbance_bav_300m)
colnames(forest_disturbance_bav_300m) <-  c("x", "y", "disturbance_year_filtered_germany", "prediction_forestcover_germany", "disturbance_severity_germany")

# Plot data
library(tidyverse)
forest_disturbance_bav_300m %>% ggplot() + geom_tile(aes(x=x, y=y, fill=disturbance_severity_germany))

# Save data
save(forest_disturbance_bav_300m, file="data/forest_disturbance_bav_300m.rda", compress="xz")
