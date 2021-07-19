#' ## KK09 and KK10 - Anthropogenic land cover change scenarios

#' ### KK09

#' Kaplan, Jed O; Krumhardt, Kristen M (2018): The KK09 Anthropogenic Land Cover Change Scenarios for Europe and neighboring countries. 
#' PANGAEA, https://doi.org/10.1594/PANGAEA.893758, 

#' Kaplan, Jed O; Krumhardt, Kristen M; Zimmermann, Niklaus E (2009): The prehistoric and preindustrial deforestation of Europe. 
#' Quaternary Science Reviews, 28(27-28), 3016-3034, https://doi.org/10.1016/j.quascirev.2009.09.028

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/KK09_KK10/"

varname <- c("land_use_low", "land_use_high", "land_use_tech")
kk09 <- lapply(varname, function(x) raster::stack(paste0(filedir, "KK09.nc"), varname=x))
#raster::plot(kk09[[1]][[1]])

# Crop by extent of bavaria
load("data/bavaria.rda")
kk09_bav <- lapply(kk09, function(x) raster::mask(raster::crop(x, bavaria, snap="out"), bavaria))
raster::plot(kk09_bav[[1]][[1]])

# Turn into data.frame
kk09_bav <- lapply(1:3, function(x){
  dat <- as.data.frame(raster::rasterToPoints(kk09_bav[[x]]))
  dat$varname <- varname[x]
  return(dat)
  })
kk09_bav <- dplyr::bind_rows(kk09_bav)
colnames(kk09_bav) <- sub("X", "", colnames(kk09_bav))
head(kk09_bav)

# Save to file
kk09_bav <- dplyr::select(kk09_bav, colnames(kk09_bav)[c(1,2,289,3:288)])
save(kk09_bav, file="data/kk09_bav.rda", compress="xz")

#' ### KK10

#' Kaplan, Jed O; Krumhardt, Kristen M (2011): The KK10 Anthropogenic Land Cover Change scenario for the preindustrial Holocene, 
#' link to data in NetCDF format. PANGAEA, https://doi.org/10.1594/PANGAEA.871369, 

#' Kaplan, Jed O; Krumhardt, Kristen M; Ellis, Erle C; Ruddiman, William F; Lemmen, Carsten; Klein Goldewijk, Kees (2011): 
#' Holocene carbon emissions as a result of anthropogenic land cover change. The Holocene, 21(5), 775-791, https://doi.org/10.1177/0959683610386983

#filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/KK09_KK10/"
filedir <- "D:/KK09_KK10/"

#varname <- c("land_use_low", "land_use_high", "land_use_tech")
kk10 <- raster::stack(paste0(filedir, "KK10.nc"))
raster::plot(kk10[[1]])

load("data/bavaria.rda")
kk10_bav <- raster::mask(raster::crop(kk10, bavaria, snap="out"), bavaria)
raster::plot(kk10_bav[[1]])

# Turn into data.frame
kk10_bav <- as.data.frame(raster::rasterToPoints(kk10_bav))
colnames(kk10_bav) <- sub("X", "", colnames(kk10_bav))
head(kk10_bav)

# Save to file
save(kk10_bav, file="data/kk10_bav.rda", compress="xz")

