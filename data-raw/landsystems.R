#' ---
#' title: "Create landsystems data for Bavaria"
#' author: "RS-eco"
#' ---

library(tidyverse)

# Downloaded from https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/XNC5KA

landsystem_dat <- raster::raster("extdata/EU_landSystem.tif")
landsystem_dat

# Load outline of Bavaria
load("data/bavaria.rda")

# Crop by extent of bavaria
bavaria_laea <- sf::st_transform(bavaria, raster::projection(landsystem_dat))
landsystem_bav <- raster::mask(raster::crop(landsystem_dat, bavaria_laea, snap="out"), bavaria_laea)
gc()

landsystem_bav <- as.data.frame(raster::rasterToPoints(landsystem_bav))
colnames(landsystem_bav) <- c("x", "y", "landsystem")
library(magrittr)
landsystem_bav %<>% 
  mutate_at(vars(-c(x,y)), function(x) 
    factor(x, levels = c(21, 22, 23, 41, 42, 43, 51, 52, 53, 61, 62,
                         63, 31, 32, 71, 72, 74, 75, 731, 732, 733, 11,
                         12, 13, 80, 90), 
           labels=c("low-intensity settlement", "medium intensity settlement", "high intensity settlement",
                    "low-intensity forest", "medium-intensity forest", "high-intensity forest", "low-intensity grassland",
                    "medium-intensity grassland", "high-intensity grassland", "low-intensity cropland", 
                    "medium-intensity cropland", "high-intensity cropland", "extensive perm-crops", "intensive perm-crops",
                    "forest/shrubs and cropland mosaics", "forest/shrubs and grassland mosaics", 
                    "forest/shrubs and bare mosaics", "forest/shrubs and mixed agriculture mosaics",
                    "low-intensity agricultural mosaics", "medium-intensity agricultural mosaics", 
                    "high-intensity agricultural mosaics", "water body", "wetland", "glacier", "shrub",
                    "bare and rocks")))
head(landsystem_bav)
save(landsystem_bav, file="data/landsystem_bav.rda", compress="xz")
gc()

# Layerize data
landsystem_bav_large <- raster::crop(landsystem_dat, sf::st_buffer(bavaria_laea, dist=1000), snap="out")
landsystem_w <- raster::layerize(landsystem_bav_large)
system_id <- sub("X","", names(landsystem_w))

# Aggregate data
landsystem_w <- lapply(1:raster::nlayers(landsystem_w), function(x){
  dat <- raster::aggregate(landsystem_w[[x]], fact=c(10, 10), fun=sum, expand=T, na.rm=T)
  raster::mask(dat, bavaria_laea)
})
raster::plot(landsystem_w[[1]])

# Turn into data.frame & calculate percentage cover
landsystem_l <- lapply(1:length(landsystem_w), function(x){
  dat <- raster::rasterToPoints(landsystem_w[[x]]) %>% as.data.frame()
  colnames(dat) <- c("x", "y", "perc_cover")
  dat$landsystem <- system_id[x]
  return(dat)
})
landsystem_l <- bind_rows(landsystem_l)
colnames(landsystem_l)
head(landsystem_l)

# Re-structure data.frame & define categories
landsystem_perc_bav <- landsystem_l %>% 
  mutate(landsystem = as.numeric(sub("X", "", landsystem))) %>% 
  mutate(landsystem = factor(landsystem, levels = c(21, 22, 23, 41, 42, 43, 51, 52, 53, 61, 62,
                         63, 31, 32, 71, 72, 74, 75, 731, 732, 733, 11,
                         12, 13, 80, 90), 
           labels=c("low-intensity settlement", "medium intensity settlement", "high intensity settlement",
                    "low-intensity forest", "medium-intensity forest", "high-intensity forest", "low-intensity grassland",
                    "medium-intensity grassland", "high-intensity grassland", "low-intensity cropland", 
                    "medium-intensity cropland", "high-intensity cropland", "extensive perm-crops", "intensive perm-crops",
                    "forest/shrubs and cropland mosaics", "forest/shrubs and grassland mosaics", 
                    "forest/shrubs and bare mosaics", "forest/shrubs and mixed agriculture mosaics",
                    "low-intensity agricultural mosaics", "medium-intensity agricultural mosaics", 
                    "high-intensity agricultural mosaics", "water body", "wetland", "glacier", "shrub",
                    "bare and rocks"))) %>%
  tidyr::spread(landsystem, perc_cover) %>% ungroup() %>% 
  mutate_at(vars(-c(x,y)), list(~ tidyr::replace_na(., 0)))
head(landsystem_perc_bav)
# Save to file
save(landsystem_perc_bav, file="data/landsystem_perc_bav.rda", compress="xz")

# Re-sample to TK25
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
landsystem_bav_large <- raster::crop(landsystem_dat, sf::st_buffer(bavaria_laea, dist=1000), snap="out")
landsystem_bav_gk <- raster::projectRaster(landsystem_bav_large, crs=sp::CRS("+init=epsg:31468"), method="ngb")
landsystem_bav_tk4tel <- raster::resample(landsystem_bav_gk, tk4tel_r, method="ngb")
landsystem_bav_tk4tel <- raster::mask(landsystem_bav_tk4tel, tk4tel_r)
raster::plot(landsystem_bav_tk4tel)
landsystem_bav_tk4tel <- as.data.frame(raster::rasterToPoints(landsystem_bav_tk4tel))
colnames(landsystem_bav_tk4tel) <- c("x", "y", "landsystem")
head(landsystem_bav_tk4tel)

# Define categories
landsystem_bav_tk4tel <- landsystem_bav_tk4tel %>%
  mutate(landsystem = factor(landsystem, levels = c(21, 22, 23, 41, 42, 43, 51, 52, 53, 61, 62,
                                                    63, 31, 32, 71, 72, 74, 75, 731, 732, 733, 11,
                                                    12, 13, 80, 90), 
                             labels=c("low-intensity settlement", "medium intensity settlement", "high intensity settlement",
                                      "low-intensity forest", "medium-intensity forest", "high-intensity forest", "low-intensity grassland",
                                      "medium-intensity grassland", "high-intensity grassland", "low-intensity cropland", 
                                      "medium-intensity cropland", "high-intensity cropland", "extensive perm-crops", "intensive perm-crops",
                                      "forest/shrubs and cropland mosaics", "forest/shrubs and grassland mosaics", 
                                      "forest/shrubs and bare mosaics", "forest/shrubs and mixed agriculture mosaics",
                                      "low-intensity agricultural mosaics", "medium-intensity agricultural mosaics", 
                                      "high-intensity agricultural mosaics", "water body", "wetland", "glacier", "shrub",
                                      "bare and rocks")))
# Save to file
save(landsystem_bav_tk4tel, file="data/landsystem_bav_tk4tel.rda", compress="xz")

# Layerize data
landsystem_w <- raster::layerize(landsystem_bav_gk)
system_id <- sub("X","", names(landsystem_w))

# Aggregate data
landsystem_w <- lapply(1:raster::nlayers(landsystem_w), function(x){
  dat <- raster::aggregate(landsystem_w[[x]], fact=c(6, 5), fun=sum, expand=T, na.rm=T)/30*100
  dat <- raster::resample(dat, tk4tel_r, method="ngb")
  dat <- raster::mask(dat, tk4tel_r)
})
raster::plot(landsystem_w[[5]])

# Turn into data.frame & calculate percentage cover
landsystem_l <- lapply(1:length(landsystem_w), function(x){
  dat <- raster::rasterToPoints(landsystem_w[[x]]) %>% as.data.frame()
  colnames(dat) <- c("x", "y", "perc_cover")
  dat$landsystem <- system_id[x]
  return(dat)
})
landsystem_l <- bind_rows(landsystem_l)
colnames(landsystem_l)
head(landsystem_l)

# Re-structure data.frame & define categories
landsystem_perc_bav_tk4tel <- landsystem_l %>% 
  mutate(landsystem = as.numeric(sub("X", "", landsystem))) %>% 
  mutate(landsystem = factor(landsystem, levels = c(21, 22, 23, 41, 42, 43, 51, 52, 53, 61, 62,
                                                    63, 31, 32, 71, 72, 74, 75, 731, 732, 733, 11,
                                                    12, 13, 80, 90), 
                             labels=c("low-intensity settlement", "medium intensity settlement", "high intensity settlement",
                                      "low-intensity forest", "medium-intensity forest", "high-intensity forest", "low-intensity grassland",
                                      "medium-intensity grassland", "high-intensity grassland", "low-intensity cropland", 
                                      "medium-intensity cropland", "high-intensity cropland", "extensive perm-crops", "intensive perm-crops",
                                      "forest/shrubs and cropland mosaics", "forest/shrubs and grassland mosaics", 
                                      "forest/shrubs and bare mosaics", "forest/shrubs and mixed agriculture mosaics",
                                      "low-intensity agricultural mosaics", "medium-intensity agricultural mosaics", 
                                      "high-intensity agricultural mosaics", "water body", "wetland", "glacier", "shrub",
                                      "bare and rocks"))) %>%
  tidyr::spread(landsystem, perc_cover) %>% ungroup() %>% 
  mutate_at(vars(-c(x,y)), list(~ tidyr::replace_na(., 0)))
head(landsystem_perc_bav_tk4tel)
# Save to file
save(landsystem_perc_bav_tk4tel, file="data/landsystem_perc_bav_tk4tel.rda", compress="xz")
