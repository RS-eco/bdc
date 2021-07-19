#' Extract Brutvoegel Data from Bavarian Atlas Zip File
#' see: https://www.lfu.bayern.de/natur/atlas_brutvoegel/index.htm

# saP-relevante Brutvögel-Arten
rm(list=ls());
library(dplyr); library(sf)

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/atlas_brutvoegel/doc/bva_shape.zip", destfile=tmp)

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

bird_bva <- lapply(files, sf::st_read)
#bird_bva_shape <- lapply(bird_bva, function(x) dplyr::select(x, -c(geometry)))
bird_bva_shape <- plyr::rbind.fill(bird_bva_shape)
#bird_bva_geo <-  lapply(bird_bva, function(x) dplyr::select(x, geometry))
#bird_bva_shape$geom <- do.call(rbind, bird_bva_geo)
bird_bva_shape <- sf::st_as_sf(bird_bva_shape) %>% select(-c(geom))
colnames(bird_bva_shape)
plot(sf::st_geometry(bird_bva_shape %>% dplyr::filter(GermanName == "Haubentaucher")))

save(bird_bva_shape, file="data/bird_bva_shape.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))

library(magrittr)
bird_bva_shape_tk4tel <- bird_bva_shape %>% as.data.frame() %>% 
  select(GermanName, QUADRANT_M, QMP_R_GK, QMP_H_GK, TK25, QMP_R_Geo, QMP_H_Geo)
bird_bva_shape_tk4tel$n <- 1
bird_bva_shape_tk4tel$GermanName <- factor(as.character(bird_bva_shape_tk25$GermanName))
bird_bva_shape_tk4tel %<>% tidyr::spread(GermanName, n) %>% mutate_at(vars(), list(~ replace_na(.x, 0)))
save(bird_bva_shape_tk4tel, file="data/bird_bva_shape_tk4tel.rda", compress="xz")
