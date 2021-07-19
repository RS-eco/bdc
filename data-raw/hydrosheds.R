#' Hydrosheds
#' see https://www.hydrosheds.org/downloads

# HydroRivers

# Load library
library(sf)

# Set filedir
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/Hydrosheds"

# Get Hydrosheds data
hydrorivers <- sf::st_read(paste0(filedir, "/HydroRIVERS_v10.shp"))

# Intersect by Bavaria
load("data/bavaria.rda")
bavaria <- st_as_sf(bavaria)

# Identify intersections
hydrorivers_bav_lst <- sf::st_intersects(hydrorivers, bavaria)

# Subset data
rivers_bav <- hydrorivers[which(lengths(hydrorivers_bav_lst) > 0),]
plot(st_geometry(rivers_bav))

# Save to file
save(rivers_bav, file="data/rivers_bav.rda", compress="xz")

# HydroLakes

#' Messager, M.L., Lehner, B., Grill, G., Nedeva, I., Schmitt, O. (2016): 
#' Estimating the volume and age of water stored in global lakes using a geo-statistical approach. 
#' Nature Communications: 13603. doi: 10.1038/ncomms13603 (open access)

# Load library
library(sf)

# Set filedir
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/Hydrosheds"

# Get Hydrosheds data
hydrolakes <- sf::st_read(paste0(filedir, "/HydroLAKES_polys_v10.shp"))
hydrolakes_points <- sf::st_read(paste0(filedir, "/HydroLAKES_points_v10.shp"))

# Subset by country
library(dplyr)
hydrolakes <- hydrolakes %>% filter(Country == "Germany")
hydrolakes_points <- hydrolakes_points %>% filter(Country == "Germany")
gc()

# Intersect by Bavaria
load("data/bavaria.rda")
bavaria <- st_as_sf(bavaria)

# Identify intersections
hydrolakes_bav_lst <- sf::st_intersects(hydrolakes, bavaria)
hydrolakes_points_bav_lst <- sf::st_intersects(hydrolakes_points, bavaria)

# Subset data
lakes_poly_bav <- hydrolakes[which(lengths(hydrolakes_bav_lst) > 0),]
lakes_points_bav <- hydrolakes_points[which(lengths(hydrolakes_points_bav_lst) > 0),]
plot(st_geometry(lakes_poly_bav))
#plot(st_geometry(lakes_points_bav), add=T, col="red")

# Save to file
save(lakes_poly_bav, file="data/lakes_poly_bav.rda", compress="xz")
save(lakes_points_bav, file="data/lakes_points_bav.rda", compress="xz")

# HydroAtlas

#' HydroATLAS data are available for download in different formats from the figshare data repository at 
#' https://doi.org/10.6084/m9.figshare.9890531

#' ## Related publications

#' Linke, S., Lehner, B., Ouellet Dallaire, C., Ariwi, J., Grill, G., Anand, M., Beames, P., 
#' Burchard-Levine, V., Maxwell, S., Moidu, H., Tan, F., Thieme, M. (2019). 
#' Global hydro-environmental sub-basin and river reach characteristics at high spatial resolution. 
#' Scientific Data 6: 283. doi: 10.1038/s41597-019-0300-6 (open access)

# GloRiC

#' Ouellet Dallaire, C., Lehner, B., Sayre, R., Thieme, M. (2018): 
#' A multidisciplinary framework to derive global river reach classifications at high spatial resolution. 
#' Environmental Research Letters. doi: 10.1088/1748-9326/aad8e9 (open access)

# HydroBasins

#' Lehner, B., Grill G. (2013): Global river hydrography and network routing: 
#' baseline data and new approaches to study the world’s large river systems. 
#' Hydrological Processes, 27(15): 2171–2186. 
