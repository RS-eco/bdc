#' ---
#' title: "Protected area data for Bavaria"
#' author: "RS-eco"
#' ---

# See also
# https://www.lfu.bayern.de/umweltdaten/geodatendienste/pretty_downloaddienst.htm?dld=schutzgebiete

rm(list=ls()); gc()
library(sf); library(raster); library(dplyr); library(fasterize)

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/WDPA"

# Download PAs of Germany
#download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Jun2021_Public_DEU_shp.zip",
#              destfile=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp.zip"))
#download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Jun2021_Public_AUT_shp.zip",
#              destfile=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_AUT_shp.zip"))
#download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Jun2021_Public_CZE_shp.zip",
#              destfile=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_CZE_shp.zip"))
# Can be downloaded from protectedplanet.net, but is updated every month, 
# you have to adjust the month and year according to the current date.

# Unzip shapefile
#unzip(paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp.zip"), exdir = filedir)

# Read PA file
pa_deu_poly <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp_0/WDPA_WDOECM_Jun2021_Public_DEU_shp-polygons.shp"))
pa_deu_point <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp_0/WDPA_WDOECM_Jun2021_Public_DEU_shp-points.shp"))
pa_deu_poly1 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp_1/WDPA_WDOECM_Jun2021_Public_DEU_shp-polygons.shp"))
pa_deu_point1 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp_1/WDPA_WDOECM_Jun2021_Public_DEU_shp-points.shp"))
pa_deu_poly2 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp_2/WDPA_WDOECM_Jun2021_Public_DEU_shp-polygons.shp"))
pa_deu_point2 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_DEU_shp_2/WDPA_WDOECM_Jun2021_Public_DEU_shp-points.shp"))
pa_deu_poly <- bind_rows(list(pa_deu_poly, pa_deu_poly1, pa_deu_poly2)); rm(pa_deu_poly1, pa_deu_poly2); gc()
pa_deu_point <- bind_rows(list(pa_deu_point, pa_deu_point1, pa_deu_point2)); rm(pa_deu_point1, pa_deu_point2); gc()

pa_aut_poly <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_AUT_shp_0/WDPA_WDOECM_Jun2021_Public_AUT_shp-polygons.shp"))
pa_aut_point <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_AUT_shp_0/WDPA_WDOECM_Jun2021_Public_AUT_shp-points.shp"))
pa_aut_poly1 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_AUT_shp_1/WDPA_WDOECM_Jun2021_Public_AUT_shp-polygons.shp"))
pa_aut_point1 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_AUT_shp_1/WDPA_WDOECM_Jun2021_Public_AUT_shp-points.shp"))
pa_aut_poly2 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_AUT_shp_2/WDPA_WDOECM_Jun2021_Public_AUT_shp-polygons.shp"))
pa_aut_point2 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_AUT_shp_2/WDPA_WDOECM_Jun2021_Public_AUT_shp-points.shp"))
pa_aut_poly <- bind_rows(list(pa_aut_poly, pa_aut_poly1, pa_aut_poly2)); rm(pa_aut_poly1, pa_aut_poly2); gc()
pa_aut_point <- bind_rows(list(pa_aut_point, pa_aut_point1, pa_aut_point2)); rm(pa_aut_point1, pa_aut_point2); gc()
pa_aut <- bind_rows(pa_aut_poly, pa_aut_point)

pa_cze_poly <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_CZE_shp_0/WDPA_WDOECM_Jun2021_Public_CZE_shp-polygons.shp"))
pa_cze_poly1 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_CZE_shp_1/WDPA_WDOECM_Jun2021_Public_CZE_shp-polygons.shp"))
pa_cze_poly2 <- sf::st_read(dsn=paste0(filedir, "/WDPA_WDOECM_Jun2021_Public_CZE_shp_2/WDPA_WDOECM_Jun2021_Public_CZE_shp-polygons.shp"))
pa_cze_poly <- bind_rows(list(pa_cze_poly, pa_cze_poly1, pa_cze_poly2)); rm(pa_cze_poly1, pa_cze_poly2); gc()

# Add Switzerland, Poland, France, Italy and Netherlands

# Check if points lie outside polygons
pa_deu_out <- pa_deu_point[which(lengths(st_within(pa_deu_point, pa_deu_poly))==0),]
pa_aut_out <- pa_aut_point[which(lengths(st_within(pa_aut_point, pa_aut_poly))==0),]

pa_deu <- bind_rows(pa_deu_poly, pa_deu_point); rm(pa_deu_point); gc()
pa_aut <- bind_rows(pa_aut_poly, pa_aut_point); rm(pa_aut_point); gc()

#' Transform shapefile into GK projection
pa_deu_poly_gk <- st_transform(pa_deu_poly, crs=31468)
pa_deu_gk <- st_transform(pa_deu, crs=31468)

# Load outline of bavaria
load("data/bavaria.rda")

# Subset PAs by extent of Bavaria
bavaria <- sf::st_as_sf(bavaria)
bavaria_gk <- st_transform(bavaria, crs=31468)

# Identify intersections
pa_bav_lst <- sf::st_intersects(pa_deu_poly_gk, bavaria_gk)

# Subset data
pa_bav <- pa_deu_poly_gk[which(lengths(pa_bav_lst) > 0),]
unique(pa_bav$SUB_LOC)

# Check non of the left-out PAs are in Bavaria
pa_non_bav <- pa_deu_poly_gk[which(lengths(pa_bav_lst) == 0),]
unique(pa_non_bav$SUB_LOC)

#Save to file
save(pa_bav, file="data/pa_bav.rda", compress="xz")

# Load TK25 grid
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
raster::projection(tk4tel_r) <- sp::CRS("+init=epsg:31468")

# Just get IUCN categories
# Specify raster resolution (1km)
cover <- fasterize::fasterize(pa_deu_poly_gk, raster=disaggregate(tk4tel_r, fact=10), by="IUCN_CAT")
cover_all <- fasterize::fasterize(pa_deu_poly_gk, raster=disaggregate(tk4tel_r, fact=10))
cover <- aggregate(stack(cover, cover_all), fact=10, fun=sum)
names(cover)[8] <- "Total" 
plot(cover)

no_pas <- fasterize::fasterize(pa_deu_poly_gk, raster=disaggregate(tk4tel_r, fact=10), 
                               fun="count", by="IUCN_CAT")
no_pas_all <- fasterize::fasterize(pa_deu_poly_gk, raster=disaggregate(tk4tel_r, fact=10), fun="count")
no_pas <- aggregate(stack(no_pas, no_pas_all), fact=10, fun=sum)
names(no_pas)[8] <- "Total" 
plot(no_pas)

year <- fasterize::fasterize(pa_deu_poly_gk, raster=disaggregate(tk4tel_r, fact=10), 
                                 field="STATUS_YR", fun="min", by="IUCN_CAT")
year_all <- fasterize::fasterize(pa_deu_poly_gk, raster=disaggregate(tk4tel_r, fact=10), 
                             field="STATUS_YR", fun="min")
min_year <- aggregate(stack(year, year_all), fact=10, fun="min")
names(min_year)[8] <- "Total" 
plot(min_year)

# Crop by extent of bavaria
cover_pa_bav_tk4tel <- raster::mask(raster::crop(cover, tk4tel_r, snap="out"), tk4tel_r)
no_pa_bav_tk4tel <- raster::mask(raster::crop(no_pas, tk4tel_r, snap="out"), tk4tel_r)
minyear_pa_bav_tk4tel <- raster::mask(raster::crop(min_year, tk4tel_r, snap="out"), tk4tel_r)

#' Turn into data.frame
cover_pa_bav_tk4tel <- as.data.frame(raster::rasterToPoints(cover_pa_bav_tk4tel))
no_pa_bav_tk4tel <- as.data.frame(raster::rasterToPoints(no_pa_bav_tk4tel))
minyear_pa_bav_tk4tel <- as.data.frame(raster::rasterToPoints(minyear_pa_bav_tk4tel))

cover_pa_bav_tk4tel <- cover_pa_bav_tk4tel %>% tidyr::gather(iucn_cat, value, -c(x,y))
cover_pa_bav_tk4tel$var <- "perc_cov"
no_pa_bav_tk4tel <- no_pa_bav_tk4tel %>% tidyr::gather(iucn_cat, value, -c(x,y))
no_pa_bav_tk4tel$var <- "no_pa"
minyear_pa_bav_tk4tel <- minyear_pa_bav_tk4tel %>% tidyr::gather(iucn_cat, value, -c(x,y))
minyear_pa_bav_tk4tel$var <- "min_year"

#' Save shapefile as RDA file
pa_bav_tk4tel <- dplyr::bind_rows(list(cover_pa_bav_tk4tel, no_pa_bav_tk4tel, minyear_pa_bav_tk4tel))
save(pa_bav_tk4tel, file="data/pa_bav_tk4tel.rda", compress="xz")

# Load TK25 data
data("tk4tel_grid")
r_tk4tel <- raster::rasterFromXYZ(tk4tel_grid)
raster::projection(r_tk4tel) <- "+init=epsg:31468"
r_tk4tel_ext <- extend(r_tk4tel, 50)

# Calculate distance raster
pa_all <- bind_rows(pa_deu_poly, pa_aut_poly, pa_cze_poly)
pa_all <- sf::st_transform(pa_all, crs=31468)
pa_all_r <- fasterize::fasterize(pa_all, r_tk4tel_ext, by="IUCN_CAT")
names(pa_all_r)
plot(pa_all_r)
dist_pa_bav <- lapply(1:nlayers(pa_all_r), function(x) raster::distance(pa_all_r[[1]]))
dist_pa_bav <- stack(dist_pa_bav)
dist_pa_bav_total <- raster::distance(fasterize::fasterize(pa_all, r_tk4tel_ext))
dist_pa_bav <- stack(dist_pa_bav, dist_pa_bav_total)
names(dist_pa_bav) <- c(names(pa_all_r), "Total")
raster::plot(dist_pa_bav)
dist_pa_bav2 <- raster::crop(dist_pa_bav, r_tk4tel)
dist_pa_bav2 <- raster::mask(dist_pa_bav2, bavaria_gk)
raster::plot(dist_pa_bav2)
dist_pa_bav <- as.data.frame(raster::rasterToPoints(dist_pa_bav2))
dist_pa_bav <- dist_pa_bav %>% tidyr::gather(iucn_cat, value, -c(x,y))
colnames(dist_pa_bav) <- c("x", "y", "iucn_cat", "dist")
dist_pa_bav$dist <- dist_pa_bav$dist/1000
dist_pa_bav_tk4tel <- as.data.frame(raster::rasterToPoints(dist_pa_bav2))
dist_pa_bav_tk4tel <- dist_pa_bav_tk4tel %>% tidyr::gather(iucn_cat, value, -c(x,y))
colnames(dist_pa_bav_tk4tel) <- c("x", "y", "iucn_cat", "dist")
dist_pa_bav_tk4tel$dist <- dist_pa_bav_tk4tel$dist/1000
rm(pa_all, pa_all_r); invisible(gc())
save(dist_pa_bav_tk4tel, file="data/dist_pa_bav_tk4tel.rda", compress="xz")
