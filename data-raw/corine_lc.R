#' ---
#' title: "Create Corine land cover data for Bavaria"
#' author: "RS-eco"
#' ---

# Downloaded from https://land.copernicus.eu/pan-european/corine-land-cover

# Specify file directory
filedir <- "/media/matt/Data/Data/Corine/DATA"
#filedir <- "E:/Data/Corine/DATA"

# Load packages
library(raster); library(dplyr); library(magrittr)

# Load files
cha_files <- list.files(filedir, pattern=".*_CHA.*\\.tif$", recursive=T, full.names=T)
clc_files <- list.files(filedir, pattern=".*_CLC.*\\.tif$", recursive=T, full.names=T)
cha_dat <- raster::stack(cha_files)
clc_dat <- raster::stack(clc_files)

# Load outline of Bavaria
load("data/bavaria.rda")

# Crop by extent of bavaria
bavaria_laea <- sf::st_transform(bavaria, raster::projection(cha_dat))
cha_bav <- raster::mask(raster::crop(cha_dat, bavaria_laea, snap="out"), bavaria_laea)
clc_bav <- raster::mask(raster::crop(clc_dat, bavaria_laea, snap="out"), bavaria_laea)
gc()

years <- c(1990, 2000, 2006, 2012, 2018)
cha_layers <- c("9000_00", "9000_90", "0006_00", "0006_06", "0612_06", "0612_12", "1218_12", "1218_18")
corine_lc_bav <- as.data.frame(raster::rasterToPoints(clc_bav))
corine_cha_bav <- as.data.frame(raster::rasterToPoints(cha_bav))
colnames(corine_lc_bav) <- c("x", "y", years)
colnames(corine_cha_bav) <- c("x", "y", cha_layers)
corine_lc_bav %<>% mutate_at(vars(-c(x,y)), function(x) factor(x, levels = c(1:44, 48, 128), 
                                                               labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
                                                                        "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
                                                                        "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
                                                                        "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                                                                        "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
                                                                        "Land principally occupied by agriculture with significant areas of natural vegetation",
                                                                        "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
                                                                        "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
                                                                        "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
                                                                        "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
                                                                        "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", NA)))
corine_cha_bav %<>% mutate_at(vars(-c(x,y)), function(x) factor(x, levels = c(1:44, 48, 128), 
                                                                labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
                                                                         "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
                                                                         "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
                                                                         "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                                                                         "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
                                                                         "Land principally occupied by agriculture with significant areas of natural vegetation",
                                                                         "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
                                                                         "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
                                                                         "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
                                                                         "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
                                                                         "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", "NA")))
corine_cha_bav <- corine_cha_bav[rowSums(corine_cha_bav=="NA")<8,]
head(corine_lc_bav)
head(corine_cha_bav)
save(corine_lc_bav, file="data/corine_lc_bav.rda", compress="xz")
save(corine_cha_bav, file="data/corine_cha_bav.rda", compress="xz")
gc()

# Layerize data
clc_bav_large <- raster::crop(clc_dat, sf::st_buffer(bavaria_laea, dist=1000), snap="out")
# Need to use unmasked data here, so that percentage values add up to 100
clc_dat_w <- lapply(1:nlayers(clc_bav_large), function(x){
  raster::layerize(clc_bav_large[[x]])
})
clc_dat_w

# Aggregate data
clc_dat_w <- lapply(1:length(clc_dat_w), function(x){
  dat <- aggregate(clc_dat_w[[x]], fact=c(10, 10), fun=sum, expand=T, na.rm=T)
  mask(dat, bavaria_laea)
})
plot(clc_dat_w[[1]][[12]])

# Turn into data.frame & calculate percentage cover
clc_dat_l <- lapply(1:length(clc_dat_w), function(x){
  dat <- raster::rasterToPoints(clc_dat_w[[x]]) %>% as.data.frame()
  dat$year <- years[x]
  return(dat)
})
clc_dat_l <- bind_rows(clc_dat_l)
colnames(clc_dat_l)
head(clc_dat_l)

# Re-structure data.frame & define categories
corine_lc_perc_bav <- clc_dat_l %>% group_by(x,y,year) %>% 
  tidyr::gather(var, perc_cover, -group_cols()) %>% 
  mutate(var = as.numeric(sub("X", "", var))) %>% 
  mutate(var = factor(var, levels = c(1:44, 48, 128), 
                      labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
                               "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
                               "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
                               "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                               "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
                               "Land principally occupied by agriculture with significant areas of natural vegetation",
                               "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
                               "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
                               "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
                               "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
                               "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", "NA"))) %>%
  tidyr::spread(var, perc_cover) %>% ungroup() %>% mutate_at(vars(-c(x,y,year)), list(~ tidyr::replace_na(., 0)))
head(corine_lc_perc_bav)
unique(rowSums(corine_lc_perc_bav[,-c(1,2,3)]))
# Save to file
save(corine_lc_perc_bav, file="data/corine_lc_perc_bav.rda", compress="xz")

# Layerize data
cha_bav_large <- raster::crop(cha_dat, sf::st_buffer(bavaria_laea, dist=1000), snap="out")
# Need to use unmasked data here, so that percentage values add up to 100
cha_dat_w <- lapply(1:nlayers(cha_bav_large), function(x){
  raster::layerize(cha_bav_large[[x]])
})
cha_dat_w

# Aggregate data
cha_dat_w <- lapply(1:length(cha_dat_w), function(x){
  dat <- aggregate(cha_dat_w[[x]], fact=c(10, 10), fun=sum, expand=T, na.rm=T)
  mask(dat, bavaria_laea)
})
plot(cha_dat_w[[1]][[15]])

# Turn into data.frame & calculate percentage cover
cha_dat_l <- lapply(1:length(cha_dat_w), function(x){
  dat <- raster::rasterToPoints(cha_dat_w[[x]]) %>% as.data.frame()
  dat$year_fromto <- cha_layers[x]
  return(dat)
})
cha_dat_l <- bind_rows(cha_dat_l)
colnames(cha_dat_l)
head(cha_dat_l)

# Re-structure data.frame & define categories
corine_cha_perc_bav <- cha_dat_l %>% group_by(x,y,year_fromto) %>% 
  tidyr::gather(var, perc_cover, -group_cols()) %>% 
  mutate(var = as.numeric(sub("X", "", var))) %>% 
  mutate(var = factor(var, levels = c(1:44, 48, 128), 
                      labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
                               "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
                               "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
                               "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                               "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
                               "Land principally occupied by agriculture with significant areas of natural vegetation",
                               "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
                               "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
                               "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
                               "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
                               "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", "NA"))) %>%
  tidyr::spread(var, perc_cover) %>% ungroup() %>% mutate_at(vars(-c(x,y,year_fromto)), list(~ tidyr::replace_na(., 0)))
head(corine_cha_perc_bav)
corine_cha_perc_bav <- corine_cha_perc_bav %>% filter(`NA` != 100)
unique(rowSums(corine_cha_perc_bav[,-c(1,2,3)]))
# Save to file
save(corine_cha_perc_bav, file="data/corine_cha_perc_bav.rda", compress="xz")

# Re-sample to TK25
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
cha_bav_large <- raster::crop(cha_dat, sf::st_buffer(bavaria_laea, dist=1000), snap="out")
clc_bav_large <- raster::crop(clc_dat, sf::st_buffer(bavaria_laea, dist=1000), snap="out")
NAvalue(cha_bav_large) <- 128
#NAvalue(clc_bav_large) <- 128
cha_bav_gk <- raster::projectRaster(cha_bav_large, crs=sp::CRS("+init=epsg:31468"), method="ngb")
clc_bav_gk <- raster::projectRaster(clc_bav_large, crs=sp::CRS("+init=epsg:31468"), method="ngb")
corine_cha_bav_tk4tel <- raster::resample(cha_bav_gk, tk4tel_r, method="ngb")
corine_lc_bav_tk4tel <- raster::resample(clc_bav_gk, tk4tel_r, method="ngb")
corine_cha_bav_tk4tel <- raster::mask(corine_cha_bav_tk4tel, tk4tel_r)
corine_lc_bav_tk4tel <- raster::mask(corine_lc_bav_tk4tel, tk4tel_r)
raster::plot(corine_cha_bav_tk4tel)
raster::plot(corine_lc_bav_tk4tel)
corine_cha_bav_tk4tel <- as.data.frame(raster::rasterToPoints(corine_cha_bav_tk4tel))
corine_lc_bav_tk4tel <- as.data.frame(raster::rasterToPoints(corine_lc_bav_tk4tel))
colnames(corine_cha_bav_tk4tel) <- c("x", "y", cha_layers)
colnames(corine_lc_bav_tk4tel) <- c("x", "y", years)
head(corine_cha_bav_tk4tel)
head(corine_lc_bav_tk4tel)

# Define categories
corine_lc_bav_tk4tel <- corine_lc_bav_tk4tel %>%
  mutate_at(vars(-c(x,y)), factor, levels = c(1:44, 48, 128), 
            labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
                     "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
                     "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
                     "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                     "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
                     "Land principally occupied by agriculture with significant areas of natural vegetation",
                     "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
                     "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
                     "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
                     "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
                     "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", "NA"))
corine_cha_bav_tk4tel <- corine_cha_bav_tk4tel %>%
  mutate_at(vars(-c(x,y)), factor, levels = c(1:44, 48, 128), 
            labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
                     "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
                     "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
                     "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                     "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
                     "Land principally occupied by agriculture with significant areas of natural vegetation",
                     "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
                     "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
                     "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
                     "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
                     "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", "NA"))

# Save to file
save(corine_lc_bav_tk4tel, file="data/corine_lc_bav_tk4tel.rda", compress="xz")
save(corine_cha_bav_tk4tel, file="data/corine_cha_bav_tk4tel.rda", compress="xz")

library(ggplot2); library(scico)
clc_tk4tel %>% ggplot() + geom_tile(aes(x=x, y=y, fill=`Fruit trees and berry plantations`)) + 
  coord_sf() +  scale_fill_scico(palette="roma") + 
  theme(legend.position="bottom")
#ggsave("corine_lc_che.png", width=10, height=10, dpi=600)
rm(clc_dat); invisible(gc())

# Plot all variables
clc_ch %>% ggplot() +             # start plotting # type of plot, define x and y axis, definecolor as factor of nutcracker absence or presence, set shape and size
  geom_tile(aes (x=x, y=y, fill=X2018)) + # hier alle Variablen einzeln aufzählen?
  facet_wrap(.~var) + 
  scale_color_lancet(palette = c("lanonc")) + # define colors hier müsste eine Farbpalette automatisch allen Variablen zugewiesen werden…?
  coord_equal() +  ggtitle("Landcover, Switzerland") + # set plot title
  theme(legend.position = "right")

# Plot individual variable from long format
clc_ch %>% filter(var == "Mixed forest") %>% 
  ggplot() +            # start plotting # type of plot, define x and y axis, definecolor as factor of nutcracker absence or presence, set shape and size
  geom_tile(aes (x=x, y=y, fill=X2018)) + # hier alle Variablen einzeln aufzählen?
  scale_color_lancet(palette = c("lanonc")) + # define colors hier müsste eine Farbpalette automatisch allen Variablen zugewiesen werden…?
  coord_equal() +  ggtitle("Landcover, Switzerland") + # set plot title
  theme(legend.position = "right")

# Turn data into wide format
clc_ch_wide <- clc_ch %>% group_by(x,y) %>% select(var, X2018) %>% spread(var, X2018)

# Plot individual variable from wide format
clc_ch_wide %>% ggplot() +            # start plotting # type of plot, define x and y axis, definecolor as factor of nutcracker absence or presence, set shape and size
  geom_tile(aes (x=x, y=y, fill=`Mixed forest`)) + # hier alle Variablen einzeln aufzählen?
  scale_color_lancet(palette = c("lanonc")) + # define colors hier müsste eine Farbpalette automatisch allen Variablen zugewiesen werden…?
  coord_equal() +  ggtitle("Landcover, Switzerland") + # set plot title
  theme(legend.position = "right")

# Calculate dominant land-cover class
dominant_clc <- clc_ch %>% group_by(x,y) %>% slice(which.max(X2018)) %>% 
  select(x,y,var, X2018) %>% mutate(var = as.factor(var))

dominant_clc %>% ggplot() +
  geom_tile(aes(x=x, y=y, fill=var)) + 
  scale_fill_manual(name="Landcover", values=c("#ffffa8","#00a600","#ff0000","#80f2e6","#4dff00","#e6e64d",
                                               "#cccccc", "#ccf24d", "#ccffcc", "#ffe64d", "#80ff00", "#a6e6cc",
                                               "#a6f200", "#a6ff80")) + theme_bw()
