#' Potentielle natürliche Vegetation Bayern
#' see: https://www.lfu.bayern.de/natur/potentielle_natuerliche_vegetation/download_pnv/index.htm

rm(list=ls()); gc()
library(dplyr); library(sf)

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/potentielle_natuerliche_vegetation/doc/pnv_500_bayern.zip", destfile=tmp)

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

pnv_bav <- lapply(files, sf::st_read)
#pnv_bav_geo <-  lapply(pnv_bav, function(x) dplyr::select(x, geometry))
#pnv_bav <- lapply(pnv_bav, function(x) dplyr::select(x, -c(geometry)))
pnv_bav <- plyr::rbind.fill(pnv_bav)
#pnv_bav$geom <- do.call(rbind, pnv_bav_geo)
pnv_bav <- sf::st_as_sf(pnv_bav)

plot(sf::st_geometry(pnv_bav))
save(pnv_bav, file="data/pnv_bav.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
