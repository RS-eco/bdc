#' ---
#' title: "Gridded population map of Bavaria from disaggregated census data and bottom-up estimates"
#' author: "RS-eco"
#' ---

#' This dataset features three gridded population datasets of Bavaria on a 10m grid. The units are people per grid cell.

#' ## Datasets

#' DE_POP_VOLADJ16: This dataset was produced by disaggregating national census counts to 10m grid cells based on a weighted dasymetric mapping approach. A building density, building height and building type dataset were used as underlying covariates, with an adjusted volume for multi-family residential buildings.
#' DE_POP_TDBP: This dataset is considered a best product, based on a dasymetric mapping approach that disaggregated municipal census counts to 10m grid cells using the same three underyling covariate layers.
#' DE_POP_BU: This dataset is based on a bottom-up gridded population estimate. A building density, building height and building type layer were used to compute a living floor area dataset in a 10m grid. Using federal statistics on the average living floor are per capita, this bottom-up estimate was created.
#' Please refer to the related publication for details.

## Temporal extent

#' The building density layer is based on Sentinel-2 time series data from 2018 and Sentinel-1 time series data from 2017 (doi: http://doi.org/10.1594/PANGAEA.920894)
#' The building height layer is representative for ca. 2015 (doi: 10.5281/zenodo.4066295)
#' The building types layer is based on Sentinel-2 time series data from 2018 and Sentinel-1 time series data from 2017 (doi: 10.5281/zenodo.4601219)
#' The underlying census data is from 2018.

#' ## Data format

#' The data come in tiles of 30x30km (see shapefile). 
#' The projection is EPSG:3035. The images are compressed GeoTiff files (.tif). 
#' There is a mosaic in GDAL Virtual format (*.vrt), which can readily be opened in most Geographic Information Systems.

#' ## Further information

#' For further information, please see the publication or contact Franz Schug (franz.schug@geo.hu-berlin.de).

#' ## Publication

#' Schug, F., Frantz, D., van der Linden, S., & Hostert, P. (2021). Gridded population mapping for Germany based on building density, height and type from Earth Observation data using census disaggregation and bottom-up estimates. PLOS ONE. DOI: 10.1371/journal.pone.0249044

filedir <- "/home/matt/Documents/population-germany/"
list.files(filedir)

list.files(paste0(filedir, "shp"))

# Load grid
library(sf)
grid <- read_sf(paste0(filedir, "shp/grid.shp"))

# Load outline of Bavaria
load("data/bavaria.rda")

# Crop by extent of bavaria
bavaria_laea <- sf::st_transform(bavaria, st_crs(grid))
grid_bav <- sf::st_intersection(grid, bavaria_laea); gc()

plot(st_geometry(grid_bav))

de_pop_bu <- lapply(paste0(filedir, grid_bav$Tile_ID, "/DE_POP_BU.tif"), function(x) raster::raster(x))
de_pop_bu <- do.call(raster::merge, de_pop_bu); gc()

raster::plot(de_pop_bu)

de_pop_tdbp <- lapply(paste0(filedir, grid_bav$Tile_ID, "/DE_POP_TDBP.tif"), function(x) raster::raster(x))
de_pop_tdbp <- do.call(raster::merge, de_pop_tdbp); gc()

raster::plot(de_pop_tdbp)

de_pop_voladj16 <- lapply(paste0(filedir, grid_bav$Tile_ID, "/DE_POP_VOLADJ16.tif"), function(x) raster::raster(x))
de_pop_voladj16 <- do.call(raster::merge, de_pop_voladj16); gc()

raster::plot(de_pop_voladj16)

# Change resolution to 1km
de_pop_bu_1km <- raster::aggregate(de_pop_bu, fact=100, fun=sum, na.rm=T); rm(de_pop_bu); gc()
de_pop_tdbp_1km <- raster::aggregate(de_pop_tdbp, fact=100, fun=sum, na.rm=T); rm(de_pop_tdbp); gc()
de_pop_voladj16_1km <- raster::aggregate(de_pop_voladj16, fact=100, fun=sum, na.rm=T); rm(de_pop_voladj16); gc()

bav_pop_1km <- raster::stack(de_pop_bu_1km, de_pop_tdbp_1km, de_pop_voladj16_1km)
names(bav_pop_1km) <- c("DE_POP_BU", "DE_POP_TDBP", "DE_POP_VOLADJ16")
rm(de_pop_bu_1km, de_pop_tdbp_1km, de_pop_voladj16_1km); gc()

raster::plot(bav_pop_1km)

# Crop by extent of Bavaria
bav_pop_1km <- raster::mask(raster::crop(bav_pop_1km, bavaria_laea, snap="out"), bavaria_laea)

# Turn into data.frame
bav_pop_1km <- as.data.frame(raster::rasterToPoints(bav_pop_1km))
colnames(bav_pop_1km)

# Save to file
save(bav_pop_1km, file="data/bav_pop_1km.rda", compress="xz")
