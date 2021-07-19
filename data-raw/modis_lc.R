#' ---
#' title: "MODIS land cover for Bavaria"
#' author: "RS-eco"
#' ---

# MODIS High-resolution Country-wise Landcover

#' MCD12Q1 the Yearly Tile Land Cover Type product from MODIS
#' Combined with a ground resolution of 500m

#' Download link to MCD12Q1 Product
#https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.051/2001.01.01/
#' You need to have a EarthData account to download the file

#' Identify required tiles
library(MODIS)
tiles <- getTile("Germany")

gadm <- get(load("data/bavaria.rda"))

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/MODIS/MCD12Q1/"

# Specify years
years <- 2001:2013

#' List correct files and turn into raster stacks
library(gdalUtils); library(raster)
lc_years <- lapply(years, function(y){
  file <- unlist(lapply(tiles@tile, function(t)
    list.files(filedir, pattern=paste0("MCD12Q1.A", y, "001.", t), full.names=T)))
  file <- file[!grepl(file, pattern=".hdf.xml")]

  #' Convert HDF File to raster stack
  data <- lapply(file, get_subdatasets)
  data <- lapply(data, function(dat){
    sub <- lapply(c(1:6,11,12), function(x){
      tmp <- rasterTmpFile()
      extension(tmp) <- "tif"
      gdal_translate(dat[x], dst_dataset = tmp)
      raster(tmp)
    })
    proj <- projection(sub[[1]])
    gadm <- spTransform(gadm, proj)
    sub <- stack(sub)
    sub <- crop(sub, gadm, snap="out")
  })

  # Merge to one layer
  data <- do.call(merge, data)

  # Set names
  names(data) <- c("Land_Cover_Type_1", "Land_Cover_Type_2", "Land_Cover_Type_3",
                   "Land_Cover_Type_4", "Land_Cover_Type_5", "Land_Cover_Type_1_Assessment",
                   "Land_Cover_Type_QC", "Land_Cover_Type_1_Secondary")
  return(data)
})

# Re-project to GK projection
lc_years <- lapply(lc_years, function(x) raster::projectRaster(x, crs=sp::CRS("+init=epsg:31468")))

# Calculate proportion of each class for TK25 grid
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)

#' Resample to correct resolution
#' using NGB for factor and bilinear for numeric values
lc_years <- lapply(lc_years, function(x){
  stack(lapply(1:nlayers(x), function(z){
    if(z %in% c(1:5,8)){
      raster::resample(x[[z]], y=tk4tel_r, method="ngb")
    } else{
      raster::resample(x[[z]], y=tk4tel_r, method="bilinear")
    }
  }))
})
lc_years <- lapply(lc_years, function(x) raster::mask(x, sp::spTransform(gadm, sp::CRS("+init=epsg:31468"))))

#' Turn into data.frame
modis_lc_bav_tk4tel <- lapply(1:length(lc_years), function(x){
  dat <- as.data.frame(rasterToPoints(lc_years[[x]]))
  dat$year <- years[x]
  return(dat)
  })
modis_lc_bav_tk4tel <- dplyr::bind_rows(modis_lc_bav_tk4tel)

#' Define land cover classes
library(magrittr)
modis_lc_bav_tk4tel %<>% dplyr::mutate_at(c("Land_Cover_Type_1", "Land_Cover_Type_2", "Land_Cover_Type_3","Land_Cover_Type_4", 
                            "Land_Cover_Type_5", "Land_Cover_Type_1_Secondary"), as.factor) %>% 
  dplyr::mutate_at(c("Land_Cover_Type_1", "Land_Cover_Type_2", "Land_Cover_Type_1_Secondary"), factor, levels=c(0:16, 254, 255), 
                     labels=c("Water", "Evergreen Needleleaf forest", "Evergreen Broadleaf forest", "Deciduous Needleleaf forest", 
                              "Deciduous Broadleaf forest", "Mixed forest", "Closed shrublands", "Open shrublands", "Woody savannas", 
                              "Savannas", "Grasslands", "Permanent wetlands", "Croplands", "Urban and built-up", 
                              "Cropland/Natural vegetation mosaic", "Snow and ice", "Barren or sparsely vegetated",
                              "Unclassified", "Fill Value")) %>% 
  dplyr::mutate_at("Land_Cover_Type_3", factor, levels=c(0:10, 254, 255), labels=c("Water", "Grasses/Cereal crops", "Shrubs",
                   "Broadleaf crops", "Savanna", "Evergreen Broadleaf forest", "Deciduous Broadleaf forest", "Evergreen Needleleaf forest", 
                   "Deciduous Needleleaf forest", "Non-vegetated", "Urban", "Unclassified", "Fill Value")) %>% 
  dplyr::mutate_at("Land_Cover_Type_4", factor, levels=c(0:8, 254, 255), 
                                  labels=c("Water", "Evergreen Needleleaf vegetation", "Evergreen Broadleaf vegetation", 
                                           "Deciduous Needleleaf vegetation", "Deciduous Broadleaf vegetation",
                                           "Annual Broadleaf vegetation", "Annual grass vegetation", "Non-vegetated land", 
                                           "Urban", "Unclassified", "Fill Value")) %>% 
  dplyr::mutate_at("Land_Cover_Type_5", factor, levels=c(0:11, 254, 255), 
                                  labels=c("Water", "Evergreen Needleleaf trees", "Evergreen Broadleaf trees", "Deciduous Needleleaf trees", 
                                           "Deciduous Broadleaf trees", "Shrub", "Grass", "Cereal crops", "Broad-leaf crops",
                                           "Urban and built-up", "Snow and ice", "Barren or sparse vegetation", "Unclassified", "Fill Value"))

#' Save to file
save(modis_lc_bav_tk4tel, file="data/modis_lc_bav_tk4tel.rda", compress="xz")

library(tidyverse)
modis_lc_bav_tk4tel %>% dplyr::filter(year == 2013) %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=Land_Cover_Type_1)) + coord_sf()
