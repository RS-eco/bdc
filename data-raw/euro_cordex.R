#' ---
#' title: "Subset Euro-Cordex Data to extent of Bavaria"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()
library(raster); library(processNC); library(dplyr); library(lubridate); library(dismo); library(tidyr)

# Please also make sure the raster and dismo package are installed

# To run this code, you will also require the processNC package (available from Github.com/RS-eco/processNC) 
# and a Linux machine with cdo installed!

# Specify file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/"
filedir <- "/home/matt/"

# Load shapefile of Germany
germany <- raster::getData(name="GADM", country="DEU", level=1,
                           path=paste0(filedir, "GADM"))
deu <- sf::st_as_sf(germany)

# Load bavaria
load("data/bavaria.rda")

filedir <- paste0(filedir, "EURO_CORDEX")

####################

#' ### Region (center of boundaries):

#' ~ 27N - 72N, ~ 22W - 45E

#' ### For rotated polar RCMs (in rotated coordinates):

#' RotPole (198.0; 39.25)
#' TLC (331.79; 21.67)
#' Nx=106 Ny=103

#' ### Periods

#' Hindcast (ERA Interim): 1989 -2008
#' Control: 1951 - 2005 (1951- 1980, 1981 - 2010)
#' Scenario: 2006 - 2100 (2011-2040, 2041-71, 2071-2100)

# Domain
domain <- "EUR-11" # 0.11 degree

# Experiment
rcps <- c("rcp26", "rcp45", "rcp85")
# historical is not available for prAdjust and tasAdjust

# Driving model
#gcms <- c("CNRM-CERFACS-CNRM-CM5", "ICHEC-EC-EARTH", "IPSL-IPSL-CM5A-MR", "MOHC-HadGEM2-ES", "MPI-M-MPI-ESM-LR") 
gcms <- c("ICHEC-EC-EARTH", "IPSL-IPSL-CM5A-MR", "MPI-M-MPI-ESM-LR")
#' Models for which no files have been downloaded: "CCCma-CanESM2", "ECMWF-ERAINT", "IPSL-IPSL-CM5A-LR", 
#' "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-GFDL-ESM2G"

# Ensemble
#ensembles <- c("r1i1p1", "r2i1p1", "r3i1p1", "r12i1p1")
ensembles <- "r1i1p1"

# RCM Model
#rcm_models <- c("ARPEGE51", "CCLM4-8-17", "HIRHAM5", "RACMO22E", "RCA4", "REMO2009", "WRF331F")
rcm_models <- c("RACMO22E", "RCA4", "REMO2009")

# Downscaling realisation
rs <- "v1"
#rs <- c("v1", "v1a", "v2")

# Time Frequency
tm_freq <- "mon"

# Variable
#vars <- c("prAdjust", "tasAdjust", "tasmaxAdjust", "tasminAdjust")
vars <- c("prAdjust", "tasmaxAdjust", "tasminAdjust")

# Time period (of observed data???)
year_period <- c("1989-2010") # Is constant!!!

#' ## Identify available file combinations
#rcms_long <- c("CLMcom-CCLM4-8-17", "KNMI-RACMO22E", "MPI-CSC-REMO2009", "SMHI-RCA4", "DMI-HIRHAM5")
rcms_long <- c("KNMI-RACMO22E", "MPI-CSC-REMO2009", "SMHI-RCA4")

month_period <- c(paste0(seq(1951,2091, by=10), "01-", seq(1960,2100, by=10), "12"),
                  "197001-197012", "209101-209911", "209101-209912")

combinations <- expand.grid(variable=vars, gcm=gcms, ensemble=ensembles, rcp=rcps, rcm=rcms_long, rs=rs, time_period=month_period)
filenames <- combinations %>% rowwise() %>%
  do(filename = paste0(.$variable, "_", domain, "_", .$gcm, "_", .$rcp, "_", .$ensemble, "_", 
                       .$rcm, "_", .$rs, "-SMHI-DBS45-MESAN-", year_period, "_", 
                       tm_freq, "_", .$time_period, ".nc"))
combinations$filename <- unlist(filenames$filename)

avail_combinations <- combinations[sapply(filenames, function(x){x %in% list.files(filedir, pattern=".nc")}) == TRUE,]

not_incl_files <- list.files(filedir, pattern=".nc")[sapply(list.files(filedir, pattern=".nc"), function(x){
  x %in% unlist(filenames$filename)}) == FALSE]
length(not_incl_files)

### This should be 0!!!   

# Create names for checking which files are missing

#avail_combs <- avail_combinations %>% filter(variable == "prAdjust") %>% dplyr::select(c(gcm, rcp, rcm, ensemble, rs))
#avail_combs <- avail_combs %>% tidyr::separate(rcm, c("rcm1", "rcm2"), sep="-", remove=F)
#avail_combs$rcm1

#avail_combs <- avail_combs %>% tidyr::unite("name_new", c("rcm1", "gcm", "rcp", "ensemble", "rs"), sep=".", remove=F)
#avail_names <- data.frame(name_new = avail_combs$name_new)
#avail_names <- avail_names %>% arrange(name_new)
#avail_names <- data.frame(unique(avail_names$name_new))
#avail_names

#' ## Crop files by extent of Germany
avail_combinations$filename <- paste0(filedir, "/", avail_combinations$filename)
avail_combinations$outfile <- sub("EURO_CORDEX", "EURO_CORDEX/DEU_Output", sub("EUR-11", "DEU", avail_combinations$filename))
if(!dir.exists(paste0(filedir, "/DEU_Output"))){dir.create(paste0(filedir, "/DEU_Output"))}
lapply(1:nrow(avail_combinations), function(x){
  if(!file.exists(avail_combinations$outfile[x])){
    cropNC(file=avail_combinations$filename[x], 
           ext=c(as.vector(sp::bbox(germany))),
           outfile=avail_combinations$outfile[x])
  }
})
raster::plot(raster::stack(avail_combinations$outfile[1], varname="prAdjust")[[1]])

#' ## Mask data by Bavaria and turn data into data.frames
if(!dir.exists(paste0(filedir, "/Bav_Output"))){dir.create(paste0(filedir, "/Bav_Output"))}
lapply(1:nrow(avail_combinations), function(x){
  #print(x)
  if(!file.exists(gsub("DEU", "Bav", sub(".nc",".csv.xz", avail_combinations$outfile[x])))){
    if(!file.exists(sub(".nc", "_remap.nc", avail_combinations$outfile[x]))){
      remapNC(gridfile=list.files(paste0(system.file(package="processNC"), "/extdata"), 
                                  pattern="euro-cordex_grid_bav.txt", full.names=T), infile=avail_combinations$outfile[x],
              outfile=sub(".nc", "_remap.nc", avail_combinations$outfile[x]))
    }
    raster::stack(sub(".nc", "_remap.nc", avail_combinations$outfile[x]), 
                  varname = paste(avail_combinations$variable[x])) %>% 
      raster::mask(bavaria) %>% raster::rasterToPoints() %>%
      as.data.frame() %>% tidyr::gather(key="time", value="value", -c(x,y)) %>% 
      readr::write_csv(gsub("DEU", "Bav", sub(".nc",".csv.xz", avail_combinations$outfile[x])))
    if(x != 1){
      file.remove(sub(".nc", "_remap.nc", avail_combinations$outfile[x]))   
    }
  }
})
raster::plot(raster::stack(sub(".nc", "_remap.nc", avail_combinations$outfile[1]), varname="prAdjust")[[1]])
sp::plot(bavaria, add=T)
readr::read_csv(gsub("DEU", "Bav", sub(".nc",".csv.xz", avail_combinations$outfile[10]))) %>% 
  filter(time == "X1951.01.16") %>% 
  ggplot2::ggplot() + ggplot2::geom_tile(ggplot2::aes(x=x, y=y, fill=value)) + ggplot2::coord_map()

#' ## Merge data files of subsequent years
avail_combinations$outfile <- sub(".nc", ".csv.xz", sub("EURO_CORDEX", "EURO_CORDEX/Bav_Output", 
                                                        sub("EUR-11", "Bav", avail_combinations$filename)))
colnames(avail_combinations)
comb_files <- avail_combinations %>% ungroup() %>% dplyr::select(-c(time_period,filename)) %>% 
  group_by(variable, gcm, ensemble, rcp, rcm, rs) %>% tidyr::nest()
comb_files$infiles <- sapply(1:nrow(comb_files), function(x){
  list(comb_files$data[[x]]$outfile)})
comb_files$time_period <- "1951_2100"

comb_files$outfile <- sapply(1:nrow(comb_files), function(x){
  paste0(filedir, "/Bav_Output/", comb_files$variable, "_Bav_", comb_files$gcm, "_", comb_files$rcp, "_",
         comb_files$ensemble, "_", comb_files$rcm, "_", comb_files$rs, "-SMHI-DBS45-MESAN-", year_period, "_", 
         tm_freq, "_", comb_files$time_period, ".csv.xz")})

# Read in all files of subsequent time periods and write to one file
lapply(1:nrow(comb_files), function(x){
  print(x)
  if(!file.exists(comb_files$outfile[x])){
    #if(any(file.exists(unlist(comb_files$infiles[x])) == TRUE)){
    dat <- vroom::vroom(unlist(comb_files$infiles[x]), id="path")
    vroom::vroom_write(dat, comb_files$outfile[x])
    rm(dat); invisible(gc())
    #}
  }
})

# Read all precipitation files of Bavaria
cordex_prAdjust_bav <- dplyr::bind_rows(lapply(which(comb_files$variable== "prAdjust"), function(x){
  vroom::vroom(comb_files$outfile[[x]], id="outfile") %>%
    mutate(time = as.Date(gsub("[.]", "-", sub("X", "", time))), value=value*86400)})) %>% # Convert precipitation from kg m-2 s-1 to mm day-1
  mutate(ndays = lubridate::days_in_month(time)) %>% 
  mutate(value = value*ndays) %>% dplyr::select(-ndays) # Convert mm day-1 to mm month-1!
summary(cordex_prAdjust_bav)

# Add variable information
cordex_prAdjust_bav <- cordex_prAdjust_bav %>% left_join(avail_combinations, by=c("path"="outfile")) %>%
  dplyr::select(x,y,time,value,gcm,rcp,rcm,ensemble,rs)
head(cordex_prAdjust_bav)
summary(cordex_prAdjust_bav)

# Adapt file for better storage performance
cordex_prAdjust_bav$value <- round(cordex_prAdjust_bav$value, 2)

#' Save to file
save(cordex_prAdjust_bav, file="data/cordex_prAdjust_bav.rda", compress="xz"); rm(cordex_prAdjust_bav); gc()

# Read all temperature files of Bavaria
#cordex_tasAdjust_bav <- dplyr::bind_rows(lapply(which(comb_files$variable== "tasAdjust"), function(x){
#  vroom::vroom(comb_files$outfile[[x]], id="outfile") %>% 
#    mutate(time = as.Date(gsub("[.]", "-", sub("X", "", time))), value=value-273.15)})) # Convert temperature to degree C

# Add variable information
#cordex_tasAdjust_bav <- cordex_tasAdjust_bav %>% left_join(avail_combinations, by=c("path"="outfile")) %>%
#  dplyr::select(x,y,time,value,gcm,rcp,rcm,ensemble,rs)
#head(cordex_tasAdjust_bav)
#summary(cordex_tasAdjust_bav)

# Adapt file for better storage performance
#cordex_tasAdjust_bav$value <- round(cordex_tasAdjust_bav$value, 2)

#' Save to file
#save(cordex_tasAdjust_bav, file="data/cordex_tasAdjust_bav.rda", compress="xz"); rm(cordex_tasAdjust_bav); gc()

# Read all temperature files of Bavaria
cordex_tasminAdjust_bav <- dplyr::bind_rows(lapply(which(comb_files$variable== "tasminAdjust"), function(x){
  vroom::vroom(comb_files$outfile[[x]], id="outfile") %>% 
    mutate(time = as.Date(gsub("[.]", "-", sub("X", "", time))),
           value=value-273.15)})) # Convert temperature to degree C

# Add variable information
cordex_tasminAdjust_bav <- cordex_tasminAdjust_bav %>% left_join(avail_combinations, by=c("path"="outfile")) %>%
  dplyr::select(x,y,time,value,gcm,rcp,rcm,ensemble,rs)
head(cordex_tasminAdjust_bav)
summary(cordex_tasminAdjust_bav)

# Adapt file for better storage performance
cordex_tasminAdjust_bav$value <- round(cordex_tasminAdjust_bav$value, 2)

#' Save to file
save(cordex_tasminAdjust_bav, file="data/cordex_tasminAdjust_bav.rda", compress="xz"); rm(cordex_tasminAdjust_bav); gc()

# Read all temperature files of Bavaria
cordex_tasmaxAdjust_bav <- dplyr::bind_rows(lapply(which(comb_files$variable== "tasmaxAdjust"), function(x){
  vroom::vroom(comb_files$outfile[[x]], id="outfile") %>% 
    mutate(time = as.Date(gsub("[.]", "-", sub("X", "", time))),
           value=value-273.15)})) # Convert temperature to degree C

# Add variable information
cordex_tasmaxAdjust_bav <- cordex_tasmaxAdjust_bav %>% left_join(avail_combinations, by=c("path"="outfile")) %>%
  dplyr::select(x,y,time,value,gcm,rcp,rcm,ensemble,rs)
head(cordex_tasmaxAdjust_bav)
summary(cordex_tasmaxAdjust_bav)

# Adapt file for better storage performance
cordex_tasmaxAdjust_bav$value <- round(cordex_tasmaxAdjust_bav$value, 2)

#' Save to file
save(cordex_tasmaxAdjust_bav, file="data/cordex_tasmaxAdjust_bav.rda", compress="xz"); rm(cordex_tasmaxAdjust_bav); gc()

########################################

## Create bioclim files

load("data/cordex_prAdjust_bav.rda")

prAdjust_30yr <- cordex_prAdjust_bav %>% 
  mutate(yr = lubridate::year(time), mon = lubridate::month(time)) %>%
  filter(yr %in% c(1991:2015, 2021:2050, 2071:2100)) %>%
  mutate(yr = ifelse(yr %in% c(1991:2015),"present", 
                     ifelse(yr %in% c(2021:2050), "future", "extfuture"))) %>% 
  mutate(yr = factor(yr, levels=c("present", "future", "extfuture"), 
                     labels=c("1991-2015", "2021-2050", "2071-2100"))) %>% 
  group_by(x, y, mon, yr, gcm, rcp, rcm, ensemble, rs) %>% 
  summarise(mn=mean(value), err=sd(value), mini=min(value), maxi=max(value))
rm(cordex_prAdjust_bav); invisible(gc())

load("data/cordex_tasminAdjust_bav.rda")

tasminAdjust_30yr <- cordex_tasminAdjust_bav %>% 
  mutate(yr = lubridate::year(time), mon = lubridate::month(time)) %>%
  filter(yr %in% c(1991:2015, 2021:2050, 2071:2100)) %>%
  mutate(yr = ifelse(yr %in% c(1991:2015),"present", 
                     ifelse(yr %in% c(2021:2050), "future", "extfuture"))) %>% 
  mutate(yr = factor(yr, levels=c("present", "future", "extfuture"), 
                     labels=c("1991-2015", "2021-2050", "2071-2100"))) %>% 
  group_by(x, y, mon, yr, gcm, rcp, rcm, ensemble, rs) %>% 
  summarise(mn=mean(value), err=sd(value), mini=min(value), maxi=max(value))
rm(cordex_tasminAdjust_bav); invisible(gc())

load("data/cordex_tasmaxAdjust_bav.rda")

tasmaxAdjust_30yr <- cordex_tasmaxAdjust_bav %>% 
  mutate(yr = lubridate::year(time), mon = lubridate::month(time)) %>%
  filter(yr %in% c(1991:2015, 2021:2050, 2071:2100)) %>%
  mutate(yr = ifelse(yr %in% c(1991:2015),"present", 
                     ifelse(yr %in% c(2021:2050), "future", "extfuture"))) %>% 
  mutate(yr = factor(yr, levels=c("present", "future", "extfuture"), 
                     labels=c("1991-2015", "2021-2050", "2071-2100"))) %>% 
  group_by(x, y, mon, yr, gcm, rcp, rcm, ensemble, rs) %>% 
  summarise(mn=mean(value), err=sd(value), mini=min(value), maxi=max(value))
rm(cordex_tasmaxAdjust_bav); invisible(gc())

prec <- prAdjust_30yr %>% ungroup() %>% dplyr::select(x,y,mon,yr,gcm,rcp,rcm,ensemble,rs,mn) %>% 
  group_split(yr, gcm, rcp, rcm, ensemble, rs, keep=F)
tasmin <- tasminAdjust_30yr %>% ungroup() %>% dplyr::select(x,y,mon,yr,gcm,rcp,rcm,ensemble,rs,mn) %>% 
  group_split(yr, gcm, rcp, rcm, ensemble, rs, keep=F); rm(tasminAdjust_30yr)
tasmax <- tasmaxAdjust_30yr %>% ungroup() %>% dplyr::select(x,y,mon,yr,gcm,rcp,rcm,ensemble,rs,mn) %>% 
  group_split(yr, gcm, rcp, rcm, ensemble, rs, keep=F); rm(tasmaxAdjust_30yr)
prec <- lapply(prec, function(z){z %>% group_by(x,y) %>% tidyr::spread(mon, mn)})
prec_xy <- lapply(prec, function(z){z %>% ungroup() %>% dplyr::select(c(x,y))})
prec <- lapply(prec, function(z){z %>% ungroup() %>% dplyr::select(-c(x,y))})
tasmin <- lapply(tasmin, function(z){z %>% group_by(x,y) %>% tidyr::spread(mon, mn) %>% ungroup() %>% dplyr::select(-c(x,y))})
tasmax <- lapply(tasmax, function(z){z %>% group_by(x,y) %>% tidyr::spread(mon, mn) %>% ungroup() %>% dplyr::select(-c(x,y))})
bioclim <- lapply(1:length(prec), function(x){dismo::biovars(as.matrix(prec[[x]]), as.matrix(tasmin[[x]]), as.matrix(tasmax[[x]]))})
rm(prec, tasmin, tasmax); invisible(gc())
cordex_bioclimAdjust_30yr_bav <- lapply(1:length(bioclim), function(x){
  dat <- as.data.frame(bioclim[[x]])
  dat$x <- prec_xy[[x]]$x
  dat$y <- prec_xy[[x]]$y
  return(dat)
}); rm(bioclim); invisible(gc())
group_key <- prAdjust_30yr %>% ungroup() %>% dplyr::select(mon,yr,gcm,rcp,rcm,ensemble,rs,mn) %>% 
  group_keys(yr, gcm, rcp, ensemble, rcm, rs) %>% tidyr::unite(col="id", yr, gcm, rcp, rcm, ensemble, rs, sep="_", remove=F)
rm(prAdjust_30yr); invisible(gc())
head(cordex_bioclimAdjust_30yr_bav)
names(cordex_bioclimAdjust_30yr_bav) <- group_key$id
cordex_bioclimAdjust_30yr_bav <- bind_rows(cordex_bioclimAdjust_30yr_bav, .id="id") %>% 
  left_join(group_key, by="id") %>% dplyr::select(-id)
head(cordex_bioclimAdjust_30yr_bav)
summary(cordex_bioclimAdjust_30yr_bav)

#' Save to file
save(cordex_bioclimAdjust_30yr_bav, file="data/cordex_bioclim_bav.rda", compress="xz")
rm(cordex_bioclimAdjust_30yr_bav); gc()

#' Create bioclim data with TK25 resolution

load("data/cordex_bioclim_bav.rda")
library(dplyr)
group_key <- cordex_bioclimAdjust_30yr_bav %>% ungroup() %>% dplyr::select(yr,gcm,rcp,rcm,ensemble,rs) %>% 
  group_keys(yr, gcm, rcp, ensemble, rcm, rs) %>% 
  tidyr::unite(col="id", yr, gcm, rcp, rcm, ensemble, rs, sep="_", remove=F)
dat <- cordex_bioclimAdjust_30yr_bav %>% ungroup() %>% group_split(yr, gcm, rcp, rcm, ensemble, rs, keep=F)
dat <- lapply(dat, function(z) dplyr::select(z, c(x,y,bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)))
dat <- lapply(dat, raster::rasterFromXYZ)
dat <- lapply(dat, function(x){
  raster::projection(x) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  return(x)
})
dat <- lapply(dat, function(x) raster::projectRaster(x, crs=sp::CRS("+init=epsg:31468")))
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
dat <- lapply(dat, function(x) raster::resample(x, tk4tel_r, method="bilinear"))
dat <- lapply(dat, function(x) raster::mask(x, tk4tel_r))
dat <- lapply(dat, function(x) as.data.frame(raster::rasterToPoints(x)))
names(dat) <- group_key$id
dat <- bind_rows(dat, .id="id") %>% 
  left_join(group_key, by="id") %>% dplyr::select(-id)
colnames(dat)[22] <- "time_frame"
cordex_bioclim_bav_tk4tel  <- dat %>% dplyr::select(x, y, gcm, ensemble, rcm, rs, rcp, time_frame, bio1, bio2, bio3, bio4, bio5, bio6, bio7,
                                           bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19)
head(cordex_bioclim_bav_tk4tel)
save(cordex_bioclim_bav_tk4tel, file="data/cordex_bioclim_bav_tk4tel.rda", compress="xz")
#readr::write_csv(cordex_bioclim_bav_tk4tel, "data/cordex_bioclim_bav_tk4tel.csv.xz")
