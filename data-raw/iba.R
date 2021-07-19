#' ---
#' title: "Important bird areas of Bavaria"
#' author: "RS-eco"
#' ---

# https://bergenhusen.nabu.de/forschung/ibas/index.html

library(sf)

filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/iba/iba_bayern"
iba_bav <- sf::st_read(paste0(filedir, "/IBA_BY.shp"))
plot(iba_bav)
save(iba_bav, file="data/iba_bav.rda", compress="xz")
