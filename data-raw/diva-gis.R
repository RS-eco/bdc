#' ## Diva-GIS Data 

#' Downloaded from the Diva-GIS Website (http://www.diva-gis.org/gdata)

filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/diva-gis"

# Load German Water Data 
diva_water_areas_deu <- sf::st_read(paste0(filedir, "/DEU_water_areas_dcw.shp"))
diva_water_lines_deu <- sf::st_read(paste0(filedir, "/DEU_water_lines_dcw.shp"))
# Water lines is similar to the hydrosheds data

plot(sf::st_geometry(diva_water_lines_deu))
plot(sf::st_geometry(diva_water_areas_deu))

# Save water lines, areas
save(diva_water_lines_deu, file=paste0(filedir, "/diva_water_lines_deu.rda"), compress="xz")
save(diva_water_areas_deu, file=paste0(filedir, "/diva_water_areas_deu.rda"), compress="xz")

# Load land-cover data
diva_cover_deu <- raster::stack(paste0(filedir, "/DEU_cov.grd"))
raster::plot(diva_cover_deu)
diva_cover_deu <- as.data.frame(raster::rasterToPoints(diva_cover_deu))
# Save data
save(diva_cover_deu, file="data/diva_cover_deu.rda", compress="xz")

# Load population data
diva_pop_deu <- raster::stack(paste0(filedir, "/deu_pop.grd"))
raster::plot(diva_pop_deu)
diva_pop_deu <- as.data.frame(raster::rasterToPoints(diva_pop_deu))
# Save data
save(diva_pop_deu , file="data/diva_pop_deu.rda", compress="xz")

# Load German road and railroad data
diva_roads_deu <- sf::st_read(paste0(filedir, "/DEU_roads.shp"))
diva_rails_deu <- sf::st_read(paste0(filedir, "/DEU_rails.shp"))

# Save roads and rails of Germany
save(diva_roads_deu, file="data/diva_roads_deu.rda", compress="xz")
save(diva_rails_deu, file="data/diva_rails_deu.rda", compress="xz")
