#' ## Extract range data for Bavaria (IUCN, GARD, Birdlife)

filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/"

library(sf)

#' IUCN data was downloaded from: https://www.iucnredlist.org/resources/spatial-data-download, 
#' by running the following code:

#if(!dir.exists("ODONATA")) {
#  download.file("http://spatial-data.s3.amazonaws.com/groups/FW_ODONATA.zip", 
#                destfile = paste0(filedir, "/IUCN/FW_ODONATA.zip")) # <-- 594.3 MB
#  unzip(paste0(filedir, "/IUCN/FW_ODONATA.zip"), exdir = paste0(filedir, "/IUCN"))
#  unlink(paste0(filedir, "/IUCN/FW_ODONATA.zip"))
#}

#' You have one shapefile for a group of animals, consisting of individual polygons 
#' for each species with different information (presence, origin, seasonal). 
#' You can specify the resolution in degrees, here we use 0.11°.

#+ load_odonata, eval=F
taxa <- c("AMPHIBIANS", "REPTILES", "TERRESTRIAL_MAMMALS", "FW_ODONATA")

dsn <- paste0(filedir, "/IUCN/", taxa, ".shp")
sp_ind_shp <- lapply(dsn, sf::st_read)

# load_bavaria
load("data/bavaria.rda")
bavaria <- sf::st_as_sf(bavaria)

# Identify intersections
sp_ind_bav_lst <- lapply(sp_ind_shp, function(x) sf::st_intersects(x, bavaria))

# Subset data
sp_ind_bav <- lapply(1:4, function(x) sp_ind_shp[[x]][which(lengths(sp_ind_bav_lst[[x]]) > 0),])
plot(st_geometry(bavaria))
plot(st_geometry(sp_ind_bav[[1]]), add=T, col="red")
plot(st_geometry(bavaria), add=T)

# Crop data by bavaria
sp_ind_bav[[1]] <- as(sp_ind_bav[[1]], "Spatial") %>% rgeos::gBuffer(byid=TRUE, width=0) %>% st_as_sf()
sp_ind_bav <- lapply(sp_ind_bav, function(x) sf::st_crop(x, bavaria))

plot(st_geometry(bavaria))
plot(st_geometry(sp_ind_bav[[1]][1,]), add=T, col="red")
plot(st_geometry(bavaria), add=T)

# Save to file
for(y in 1:4){
  assign(value=sp_ind_bav[[y]], x=paste0(sub("terrestrial_", "", sub("fw_", "", tolower(taxa[y]))), "_bav"))
}
lapply(1:4, function(y) save(list=paste0(sub("terrestrial_", "", sub("fw_", "", tolower(taxa[y]))), "_bav"), 
                             file=paste0("data/", sub("terrestrial_", "", sub("fw_", "", tolower(taxa[y]))), "_bav.rda"), compress="xz"))


#' extract birdlife data

# load_bavaria
load("data/bavaria.rda")
bavaria <- sf::st_as_sf(bavaria)

dsn <- paste0(filedir, "/BirdLife_2018")
files <- list.files(dsn, pattern=".shp", full.names=T)

sp_ind_bav <- lapply(files, function(x){
  dat <- sf::st_read(x)
  # Identify intersections
  dat_bav_lst <- sf::st_intersects(dat, bavaria)
  
  # Subset data
  dat <- dat[which(lengths(dat_bav_lst) > 0),]
  
  if(nrow(dat) > 0){
    dat <- as(dat, "Spatial") %>% rgeos::gBuffer(byid=TRUE, width=0) %>% st_as_sf()
    dat <- sf::st_crop(dat,bavaria)
    return(dat)
  } else{
    return(NULL)
  }
})
sp_ind_bav <- Filter(Negate(is.null), sp_ind_bav)
bird_bav <- do.call(rbind, sp_ind_bav)
plot(st_geometry(bird_bav[1,]))
save("bird_bav", file="data/bird_bav.rda", compress="xz")

#' GARD reptiles

# Load global data
gard_reptiles <- sf::read_sf(dsn=paste0(filedir, "/GARD_ranges/modeled_reptiles.shp"))

# load_bavaria
load("data/bavaria.rda")
bavaria <- sf::st_as_sf(bavaria)

# Identify intersections
gard_reptiles_bav_lst <- sf::st_intersects(gard_reptiles, bavaria)

# Subset data
gard_reptiles_bav <- gard_reptiles[which(lengths(gard_reptiles_bav_lst) > 0),]

gard_reptiles_bav <- as(gard_reptiles_bav, "Spatial") %>% rgeos::gBuffer(byid=TRUE, width=0) %>% st_as_sf()
gard_reptiles_bav <- sf::st_crop(gard_reptiles_bav, bavaria)
plot(st_geometry(gard_reptiles_bav))

save(gard_reptiles_bav, file="data/gard_reptiles_bav.rda", compress="xz")
