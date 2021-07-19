
# Data was downloaded from:
#http://due.esrin.esa.int/page_globcover.php

# Unzip files

# Specify file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/Globcover/"

# Read files
(files <- list.files(filedir, pattern=".tif$", full.names=T))
dat <- lapply(files, function(x) raster::raster(x))

# Create identical CRS
dat <- lapply(dat, function(x){
  raster::projection(x) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  return(x)
})

# Crop by extent of bavaria
load("data/bavaria.rda")
globcover_bav <- lapply(dat, function(x) raster::crop(x, bavaria, snap="out"))
globcover_bav <- raster::stack(globcover_bav)

# Re-sample to tk4tel
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
(globcover_bav_gk <- raster::projectRaster(globcover_bav, crs=sp::CRS("+init=epsg:31468")))
globcover_bav_tk4tel <- raster::resample(globcover_bav_gk, tk4tel_r, method="ngb")
globcover_bav_tk4tel <- raster::mask(globcover_bav_tk4tel, tk4tel_r)
globcover_bav_tk4tel <- as.data.frame(raster::rasterToPoints(globcover_bav_tk25))
colnames(globcover_bav_tk4tel) <- c("x", "y", "2004_QL", 2004, "2009_QL", 2009)
head(globcover_bav_tk4tel)

# Define categories
library(dplyr)
globcover_bav_tk4tel <- globcover_bav_tk4tel %>% 
  mutate(`2009_QL`=tidyr::replace_na(`2009_QL`, 0)) %>%
  mutate_at(c("2004", "2009"), factor, levels = c(11,14,20,30,40,50,60,70,seq(90,230,by=10)), 
            labels=c("Post-flooding or irrigated croplands (or aquatic)", "Rainfed croplands",
                     "Mosaic cropland (50-70%) / vegetation (grassland/shrubland/forest) (20-50%)",
                     "Mosaic vegetation (grassland/shrubland/forest) (50-70%) / cropland (20-50%)",
                     "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)",
                     "Closed (>40%) broadleaved deciduous forest (>5m)",
                     "Open (15-40%) broadleaved deciduous forest/woodland (>5m)",
                     "Closed (>40%) needleleaved evergreen forest (>5m)",
                     "Open (15-40%) needleleaved deciduous or evergreen forest (>5m)",
                     "Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)",
                     "Mosaic forest or shrubland (50-70%) / grassland (20-50%)",
                     "Mosaic grassland (50-70%) / forest or shrubland (20-50%)",
                     "Closed to open (>15%) (broadleaved or needleleaved, evergreen or deciduous) shrubland (<5m)",
                     "Closed to open (>15%) herbaceous vegetation (grassland, savannas or lichens/mosses)",
                     "Sparse (<15%) vegetation", "Closed to open (>15%) broadleaved forest regularly flooded (semi-permanently or temporarily) - Fresh or brackish water",
                     "Closed (>40%) broadleaved forest or shrubland permanently flooded - Saline or brackish water",
                     "Closed to open (>15%) grassland or woody vegetation on regularly flooded or waterlogged soil - Fresh, brackish or saline water",
                     "Artificial surfaces and associated areas (Urban areas >50%)",
                     "Bare areas", "Water bodies", "Permanent snow and ice", "No data (burnt areas, clouds,…)"))

# Save to file
save(globcover_bav_tk4tel, file="data/globcover_bav_tk4tel.rda", compress="xz")