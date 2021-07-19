# Erb et al. 2016

#' Erb, Karl-Heinz, Tamara Fetzel, Christoph Plutzar, Thomas Kastner, Christian Lauk, Andreas Mayer, 
#' Maria Niedertscheider, Christian Korner, and Helmut Haberl. 
#' Biomass Turnover Time in Terrestrial Ecosystems Halved by Land Use. 
#' Nature Geoscience 9, no. 9 (September 2016): 67478.

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/Erb_2016/"

# Load files
files <- list.files(filedir, pattern=".tif$", full.names=T)
dat <- raster::stack(files)

#' C_reduction_perc.tif: differences in biomass stocks of potential and actual vegetation induced by land use [%]
#' C_stock_changes.tif: annual changes in biomass stocks [gC/m²/yr]
#' Turnover_Accel_WGS84.tif: Acceleration of biomass turnover
#' Turnover_Act_WGS84.tif: Actual biomass turnover [years]
#' Turnover_Pot_WGS84.tif: Potential biomass turnover [years]

# Crop by extent of bavaria
load("data/bavaria.rda")
carbon_bav <- raster::mask(raster::crop(dat, bavaria, snap="out"), bavaria)
raster::plot(carbon_bav)

# Turn into data.frame
carbon_bav <- as.data.frame(raster::rasterToPoints(carbon_bav))
colnames(carbon_bav)
head(carbon_bav)

# Save to file
save(carbon_bav, file="data/carbon_bav.rda", compress="xz")
