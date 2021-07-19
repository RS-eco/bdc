## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  results="asis", cache=TRUE,
  echo=F, warning=F, message=F,
  fig.width=10, fig.height=8
)

## ----load_pkgs----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggspatial)
library(ggsci)
library(ggthemes)
library(patchwork)
library(zoo)

# Define standard colour scheme
bluewhitered <- colorRampPalette(c("#009392","#39b185","#9ccb86","#e9e29c","#eeb479","#e88471","#cf597e"))(255)

## -----------------------------------------------------------------------------
# Load shapefile of Bavaria
data("bavaria", package="bdc")
bavaria <- sf::st_as_sf(bavaria)

## ---- fig.width=10, fig.height=5----------------------------------------------
data("amphibians_bav", package="bdc")
data("bird_bav", package="bdc")
data("gard_reptiles_bav", package="bdc")
data("mammals_bav", package="bdc")
data("odonata_bav", package="bdc")
data("reptiles_bav", package="bdc")

data("tk25_grid", package="bdc")
tk25_grid <- tk25_grid %>% raster::rasterFromXYZ()
raster::projection(tk25_grid) <- sp::CRS("+init=epsg:31468")
tk25_grid <- raster::projectRaster(tk25_grid, crs=sp::CRS("+init=epsg:4326"))

#nrow(amphibians_bav); sum(amphibians_bav$presence)

amphi <- raster::rasterize(amphibians_bav, tk25_grid) %>% raster::mask(tk25_grid) %>% 
  raster::rasterToPoints() %>% data.frame()
bird <- raster::rasterize(bird_bav, tk25_grid) %>% raster::mask(tk25_grid) %>% 
  raster::rasterToPoints() %>% data.frame()
mam <- raster::rasterize(mammals_bav, tk25_grid) %>% raster::mask(tk25_grid) %>% 
  raster::rasterToPoints() %>% data.frame()
odo <- raster::rasterize(odonata_bav, tk25_grid) %>% raster::mask(tk25_grid) %>% 
  raster::rasterToPoints() %>% data.frame()
rep <- raster::rasterize(reptiles_bav, tk25_grid) %>% raster::mask(tk25_grid) %>% 
  raster::rasterToPoints() %>% data.frame()
gard_rep <- raster::rasterize(gard_reptiles_bav, tk25_grid) %>% raster::mask(tk25_grid) %>% 
  raster::rasterToPoints() %>% data.frame()

#colnames(bird)

# Plot species richness of individual taxa
p1 <- amphi %>% ggplot() + geom_tile(aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Amphibian SR", colours=bluewhitered) + 
  geom_sf(data=bavaria, fill=NA) + coord_sf() + labs(x="", y="")
p2 <- bird %>% ggplot() + geom_tile(aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Bird SR", colours=bluewhitered) + 
  geom_sf(data=bavaria, fill=NA) + coord_sf() + labs(x="", y="")
p3 <- mam %>% ggplot() + geom_tile(aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Mammal SR", colours=bluewhitered) + 
  geom_sf(data=bavaria, fill=NA) + coord_sf() + labs(x="", y="")
p4 <- odo %>% ggplot() + geom_tile(aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Odonata SR", colours=bluewhitered) + 
  geom_sf(data=bavaria, fill=NA) + coord_sf() + labs(x="", y="")
p5 <- rep %>% ggplot() + geom_tile(aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Reptile SR", colours=bluewhitered) + 
  geom_sf(data=bavaria, fill=NA) + coord_sf() + labs(x="", y="")

p1 + p2 + p3 + p4 + p5

