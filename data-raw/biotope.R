#' Biotopkartierung Bayern
#' see: https://www.lfu.bayern.de/natur/biotopkartierung/index.htm

rm(list=ls()); gc()
library(dplyr); library(sf)

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/gdi/dls/daten/biotopkartierung/bio_fbk_epsg4258_shp.zip", destfile=tmp)

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

bio_fbk <- lapply(files, sf::st_read)

bio_fbk_geo <-  lapply(bio_fbk, function(x) dplyr::select(x, geometry))
bio_fbk <- lapply(bio_fbk, function(x) dplyr::select(x, -c(geometry)))
bio_fbk <- plyr::rbind.fill(bio_fbk)
bio_fbk$geom <- do.call(rbind, bio_fbk_geo)
bio_fbk <- sf::st_as_sf(bio_fbk)

st_crs(bio_fbk)

plot(sf::st_geometry(bio_fbk))
#save(bio_fbk, file="data/bio_fbk.rda", compress="xz")

#download.file("https://www.lfu.bayern.de/gdi/dls/daten/biotopkartierung/bio_sbk_epsg4258_shp.zip", destfile=tmp)

#download.file("https://www.lfu.bayern.de/gdi/dls/daten/biotopkartierung/bio_abk_epsg4258_shp.zip", destfile=tmp)

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
