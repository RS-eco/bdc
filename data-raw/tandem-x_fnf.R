#' ## TanDEM-X Forest/Non-Forest Map - Global

#' see https://geoservice.dlr.de/web/maps/tdm:forest

#' “Martone, M.; Rizzoli, P.; Wecklich C.; González, C.; Bueso-Bello, J.L.; Valdo, P.; Schulze, D.; Zink, M.;
#' Krieger, G.; Moreira, A. The Global Forest/Non-Forest map from TanDEM-X Interferometric SAR Data.
#' Remote Sensing of Environment 2018, 205, 352–373”.


# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/TanDEM-X"
if(!dir.exists(filedir)){dir.create(filedir)}

# Download Forest-Non-Forest Map of Bavaria
#N <- 47:51
#E <- 8:14
#df <- expand.grid(N=N, E=E)
#files <- paste0("/N", df$N, "/E0", sprintf("%02d", floor(df$E/10)*10), "/TDM_FNF_20_N", df$N, "E0", sprintf("%02d", df$E), ".zip")
#lapply(files, function(x){download.file(paste0("https://download.geoservice.dlr.de/FNF50/files", x), 
#                                        destfile=paste0(filedir, "/", basename(x)))})

# Unzip files
#zipfiles <- list.files(filedir, pattern=".zip", full.names=T)
#lapply(zipfiles, function(x) unzip(x, exdir=filedir))

# Read files
datfiles <- list.files(paste0(filedir, "/FNF"), pattern=".tiff", full.names = T)
tdm_fnf_bav <- lapply(datfiles, raster::stack)
tdm_fnf_bav <- do.call(raster::merge, tdm_fnf_bav)

# Crop by extent of bavaria
load("data/bavaria.rda")
tdm_fnf_bav <- raster::mask(raster::crop(tdm_fnf_bav, bavaria), bavaria, snap="out")
raster::plot(tdm_fnf_bav)

# Turn into data.frame
tdm_fnf_bav <- as.data.frame(raster::rasterToPoints(tdm_fnf_bav))
colnames(tdm_fnf_bav) <- c("x", "y", "fnf")
head(tdm_fnf_bav)

# Add factor values to fnf
tdm_fnf_bav$fnf <- factor(tdm_fnf_bav$fnf, levels=c(0,1,2), labels=c("Water/Settlements/Invalid", "Forest", "Non-Forest"))

#library(ggplot2)
#ggplot() + geom_raster(data=tdm_fnf_bav, aes(x=x,y=y,fill=fnf)) + coord_sf()

# Save to file
save(tdm_fnf_bav, file="data/tdm_fnf_bav.rda", compress="xz")
