#' ---
#' title: "Process mundialis MODIS LST data for Bavaria"
#' author: "RS-eco"
#' ---

# Only selected bioclim variables are available online

# Specify file directory
filedir <- "/home/matt/Documents/Data/mundialis/"

# Specify filenames
df=expand.grid(vars=c("min", "max", "avg"), year=2003:2016)
df$files <- paste0("lst_", df$year, "_", df$vars, ".zip")

# Download files
lapply(df$files, function(file){
  if(!file.exists(paste0(filedir, file))){
    download.file(paste0("https://zenodo.org/record/1115666/files/", file, "?download=1"), 
                  destfile=paste0(filedir, file))
  }
})
#https://zenodo.org/record/1115666/files/lst_2007_avg.zip?download=1

#' Unzip files
#lapply(df$files, function(file){unzip(paste0(filedir, file), exdir = filedir)})


#' Load files
dat <- raster::stack(list.files(paste0(filedir, "LST_monthly/"), pattern=".tif", full.names=T))
raster::plot(dat[[1]])

# Crop by extent of bavaria
load("data/bavaria.rda")
ext_bav <- c(5, 15, 45, 55)
dat_bav <- raster::crop(dat, ext_bav, snap="out")

load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
(dat_bav_gk <- raster::projectRaster(dat_bav, crs=sp::CRS("+init=epsg:31468")))
dat_bav_tk4tel <- raster::resample(dat_bav_gk, tk4tel_r, method="bilinear")
raster::plot(dat_bav_tk4tel)
dat_bav_tk25 <- raster::mask(dat_bav_tk4tel, tk4tel_r)
raster::plot(dat_bav_tk4tel)
modis_lst_bav_tk4tel <- as.data.frame(raster::rasterToPoints(dat_bav_tk4tel))

colnames(modis_lst_bav_tk4tel) <- sub("LST_", "", colnames(modis_lst_bav_tk4tel))
head(modis_lst_bav_tk4tel)

# Change format of data

# Calculate bioclimatic variables

# Save to file
save(modis_lst_bav_tk4tel, file="data/modis_lst_bav_tk4tel.rda", compress="xz")

