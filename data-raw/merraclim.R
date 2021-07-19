#' ---
#' title: "Process MerraClim data for Bavaria"
#' author: "RS-eco"
#' ---

# Set file directory
filedir <- "/home/matt/Documents/Data/MERRAclim/"

# Create file names
df <- expand.grid(vars=c("min", "max", "mean"), years= c("00s", "90s", "80s"), res= c("2_5m", "5m", "10m"))
df$files <- paste0(df$res, "_", df$vars, "_", df$years, ".zip")

# Download files
#lapply(df$files, function(file){
#  if(!file.exists(paste0(filedir, file))){
#    download.file(paste0("https://www.datadryad.org/bitstream/handle/10255/dryad.150291/", file, "?sequence=1"), 
#                  destfile=paste0(filedir, file))
#  }
#})
#https://www.datadryad.org/bitstream/handle/10255/dryad.150277/10m_mean_90s.zip?sequence=1
#https://www.datadryad.org/bitstream/handle/10255/dryad.150298/2_5m_min_90s.zip?sequence=1
# Files need to be downloaded manually, as dryad.number always changes!!!

#' Unzip files
#lapply(df$files, function(file){unzip(paste0(filedir, file), exdir = filedir)})

#' Load files
dat <- lapply(df$files, function(file){raster::stack(list.files(filedir, pattern=paste0("^", sub(".zip", "", file)), full.names=T)[1:19])})
raster::plot(dat[[10]])

# Crop by extent of bavaria
ext_bav <- c(5, 15, 45, 55)
dat_bav <- lapply(dat, function(x) raster::crop(x, ext_bav, snap="out"))

load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
dat_bav_gk <- lapply(dat_bav, function(x) raster::projectRaster(x, crs=sp::CRS("+init=epsg:31468")))
dat_bav_tk4tel <- lapply(dat_bav_gk, function(x) raster::resample(x, tk4tel_r, method="bilinear"))
raster::plot(dat_bav_tk4tel[[1]])
dat_bav_tk4tel <-  lapply(dat_bav_tk4tel, function(x) raster::mask(x, tk4tel_r))
raster::plot(dat_bav_tk4tel[[1]])
merraclim_bav_tk4tel <- lapply(1:length(dat_bav_tk4tel), function(x){
  dat <- as.data.frame(raster::rasterToPoints(dat_bav_tk4tel[[x]]))
  dat$var <- df$vars[[x]]
  dat$year <- df$years[[x]]
  dat$res <- df$res[[x]]
  colnames(dat) <- sub(paste0("X", df$res[[x]], "_", df$vars[[x]], "_", df$years[[x]], "_"), "", colnames(dat))
  return(dat)
})
merraclim_bav_tk4tel <- dplyr::bind_rows(merraclim_bav_tk4tel)
colnames(merraclim_bav_tk4tel)
head(merraclim_bav_tk4tel)

library(dplyr)
merraclim_2.5m_bav_tk4tel <- merraclim_bav_tk4tel %>% filter(res == "2_5m") %>% select(-res)
merraclim_5m_bav_tk4tel <- merraclim_bav_tk4tel %>% filter(res == "5m") %>% select(-res)
merraclim_10m_bav_tk4tel <- merraclim_bav_tk4tel %>% filter(res == "10m") %>% select(-res)

save(merraclim_2.5m_bav_tk4tel, file="data/merraclim_2.5m_bav_tk4tel.rda", compress="xz")
save(merraclim_5m_bav_tk4tel, file="data/merraclim_5m_bav_tk4tel.rda", compress="xz")
save(merraclim_10m_bav_tk4tel, file="data/merraclim_10m_bav_tk4tel.rda", compress="xz")

