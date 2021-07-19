#' Naturräume Bayern
#' see: https://www.lfu.bayern.de/natur/naturraeume/index.htm

rm(list=ls()); invisible(gc())
library(dplyr); library(sf)

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/naturraeume/doc/ng_geo.zip", destfile=tmp) # Biogeographic regions

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

ng_geo <- lapply(files, sf::st_read)
ng_geo <- plyr::rbind.fill(ng_geo)
ng_geo  <- sf::st_as_sf(ng_geo)
colnames(ng_geo)
plot(sf::st_geometry(ng_geo))
save(ng_geo, file="data/ng_geo.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
rm(list=ls()); invisible(gc())

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/naturraeume/doc/ng_gross.zip", destfile=tmp) # Großlandschaften

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

ng_gross <- lapply(files, sf::st_read)
#ng_gross2 <-  lapply(ng_gross, function(x) dplyr::select(x, geometry))
#ng_gross <- lapply(ng_gross, function(x) dplyr::select(x, -c(geometry)))
ng_gross <- plyr::rbind.fill(ng_gross)
#ng_gross$geom <- do.call(rbind, ng_gross2)
ng_gross  <- sf::st_as_sf(ng_gross)
colnames(ng_gross)
plot(sf::st_geometry(ng_gross))
save(ng_gross, file="data/ng_gross.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
rm(list=ls()); invisible(gc())

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/naturraeume/doc/ng_ssymank.zip", destfile=tmp) #Naturraum-Haupteinheiten

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

ng_ssymank <- lapply(files, sf::st_read)
#ng_ssymank2 <-  lapply(ng_ssymank, function(x) dplyr::select(x, geometry))
#ng_ssymank <- lapply(ng_ssymank, function(x) dplyr::select(x, -c(geometry)))
ng_ssymank <- plyr::rbind.fill(ng_ssymank)
#ng_ssymank$geom <- do.call(rbind, ng_ssymank2)
ng_ssymank  <- sf::st_as_sf(ng_ssymank)

plot(sf::st_geometry(ng_ssymank))

save(ng_ssymank, file="data/ng_ssymank.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
rm(list=ls()); invisible(gc())

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/naturraeume/doc/ng_meynen_schmit.zip", destfile=tmp) #Naturraum-Einheiten

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

ng_meynen_schmit <- lapply(files, sf::st_read)
#ng_meynen_schmit2 <-  lapply(ng_meynen_schmit, function(x) dplyr::select(x, geometry))
#ng_meynen_schmit <- lapply(ng_meynen_schmit, function(x) dplyr::select(x, -c(geometry)))
ng_meynen_schmit <- plyr::rbind.fill(ng_meynen_schmit)
#ng_meynen_schmit$geom <- do.call(rbind, ng_meynen_schmit2)
ng_meynen_schmit  <- sf::st_as_sf(ng_meynen_schmit)
colnames(ng_meynen_schmit)
plot(sf::st_geometry(ng_meynen_schmit))

save(ng_meynen_schmit, file="data/ng_meynen_schmit.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
rm(list=ls()); invisible(gc())

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/naturraeume/doc/ng_unter_absp.zip", destfile=tmp) #Naturraum-Untereinheiten (ABSP)

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

ng_unter_absp <- lapply(files, sf::st_read)
#ng_unter_absp2 <-  lapply(ng_unter_absp, function(x) dplyr::select(x, geometry))
#ng_unter_absp <- lapply(ng_unter_absp, function(x) dplyr::select(x, -c(geometry)))
ng_unter_absp <- plyr::rbind.fill(ng_unter_absp)
#ng_unter_absp$geom <- do.call(rbind, ng_unter_absp2)
ng_unter_absp <- sf::st_as_sf(ng_unter_absp)

plot(sf::st_geometry(ng_unter_absp))

save(ng_unter_absp, file="data/ng_unter_absp.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
rm(list=ls()); invisible(gc())

tmp <- tempfile(fileext=".zip")
download.file("https://www.lfu.bayern.de/natur/naturraeume/doc/ng_abspziele.zip", destfile=tmp) #Naturraum-Untereinheiten (ABSP-Ziele)

tmp_dir <- tempdir()
dat <- unzip(tmp, exdir=tmp_dir)

files <- dat[grep(dat, pattern=".shp$")]

ng_abspziele <- lapply(files, sf::st_read)
#ng_abspziele2 <-  lapply(ng_abspziele, function(x) dplyr::select(x, geometry))
#ng_abspziele <- lapply(ng_abspziele, function(x) dplyr::select(x, -c(geometry)))
ng_abspziele <- plyr::rbind.fill(ng_abspziele)
#ng_abspziele$geom <- do.call(rbind, ng_abspziele2)
ng_abspziele <- sf::st_as_sf(ng_abspziele)

plot(sf::st_geometry(ng_abspziele))

save(ng_abspziele, file="data/ng_abspziele.rda", compress="xz")

file.remove(tmp)
file.remove(list.files(tmp_dir, full.names=T, recursive=T))
rm(list=ls()); invisible(gc())
