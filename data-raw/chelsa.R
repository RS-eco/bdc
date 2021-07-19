
# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/CHELSA/"

# Specify file names (monthly climatologies)
df <- expand.grid(var=c("prec", "temp", "tmin", "tmax"),
                  months=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))

# Automatically download files
lapply(1:nrow(df), function(x){
  if(df$var[x] == "prec"){
    filename <- paste("CHELSA", df$var[x], df$months[x], "V1.2_land.tif", sep="_")
    if(!file.exists(paste0(filedir, filename))){
      download.file(paste0("https://www.wsl.ch/lud/chelsa/data/climatologies/", df$var[x], "/", filename), destfile=paste0(filedir, filename))
    }
  } else{
    filename <- paste("CHELSA", paste0(df$var[x], 10), df$months[x], "1979-2013_V1.2_land.tif", sep="_")
    if(!file.exists(paste0(filedir, filename))){
      download.file(paste0("https://www.wsl.ch/lud/chelsa/data/climatologies/temp/integer/", 
                           df$var[x], "/", filename), 
                    destfile=paste0(filedir, filename))
    }
  }
})
#https://www.wsl.ch/lud/chelsa/data/climatologies/prec/CHELSA_prec_03_V1.2_land.tif
#https://www.wsl.ch/lud/chelsa/data/climatologies/temp/integer/temp/CHELSA_temp10_01_1979-2013_V1.2_land.tif
#https://www.wsl.ch/lud/chelsa/data/climatologies/temp/integer/tmin/CHELSA_tmin10_01_1979-2013_V1.2_land.tif

# Specify file names (bioclimatic variables)
#var <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16")
bio <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16")
filename <- paste0("CHELSA_bio10_", bio, ".tif")

# Automatically download files
lapply(filename, function(x){
  if(!file.exists(paste0(filedir, x))){
    download.file(paste0("https://www.wsl.ch/lud/chelsa/data/bioclim/integer/", x), destfile=paste0(filedir, x))
  }
})

# Load files
files <- list.files(filedir, pattern=".tif", full.names=T)
chelsa <- raster::stack(files)

# Crop by extent of bavaria
load("data/bavaria.rda")
ext_bav <- c(5, 15, 45, 55)
chelsa_bav <- raster::crop(chelsa, ext_bav, snap="out")

# Re-sample to TK25
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
(chelsa_bav_gk <- raster::projectRaster(chelsa_bav, crs=sp::CRS("+init=epsg:31468")))
chelsa_bav_tk4tel <- raster::resample(chelsa_bav_gk, tk4tel_r, method="ngb")
raster::plot(chelsa_bav_tk4tel)
chelsa_bav_tk4tel <- raster::mask(chelsa_bav_tk4tel, tk4tel_r)
raster::plot(chelsa_bav_tk4tel)
chelsa_bav_tk4tel <- as.data.frame(raster::rasterToPoints(chelsa_bav_tk4tel))

colnames(chelsa_bav_tk4tel) <- gsub("_", "", sub("10_", "", sub("_1979.2013", "", sub("_V1.2_land", "", sub("CHELSA_", "", colnames(chelsa_bav_tk4tel))))))
head(chelsa_bav_tk4tel)

# Save to file
save(chelsa_bav_tk4tel, file="data/chelsa_bav_tk4tel.rda", compress="xz")
