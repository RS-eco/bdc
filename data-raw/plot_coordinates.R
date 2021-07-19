#' Extract Landklif Plot coordinates

rm(list=ls());
library(dplyr); library(sf)

filedir <- "/home/matt/t6p/group_hof/@BayKliF/data/"

library(readxl)
plot_coords_bav <- read_xlsx(paste0(filedir, "LandKlif_PlotCoordinates.xlsx"))
save(plot_coords_bav, file="data/plot_coords_bav.rda", compress="xz")
