#' ---
#' title: "Identify TK25 & TK4tel grid"
#' author: "RS-eco"
#' ---

# Info zu den Bayerischen Messtischblättern
# https://geo.dianacht.de/mtb/?proj=gk4

rm(list=ls()); gc()

# Load libraries
library(tidyverse); library(sf)

# Get grid from database

# Load ASK database
#my_db <- DBI::dbConnect(RSQLite::SQLite(), "extdata/ASK.db")
#src_tbls(my_db)

# Load background grid
#tk25_db <- dplyr::tbl(my_db, "geo_tk25_quadranten") %>% dplyr::collect()
#readr::write_csv(tk25_db, "extdata/tk25.csv")

tk25_db <- read.csv("extdata/tk25.csv")
tk25_db <- tk25_db # %>% tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="/", remove = TRUE) %>% 
  #dplyr::select(-c("KARTE_QUAD"))

library(scico)
ggplot() +
  geom_rect(data=tk25_db, aes(xmin = XLU, xmax = XRU, 
                           ymin = YLU, ymax = YLO, fill=KARTE_QUAD)) + 
  scale_fill_scico(palette="roma")
ggplot() +
  geom_point(data=tk25_db, aes(x = XQMITTE, y = YQMITTE, fill=KARTE_QUAD)) + 
  scale_fill_scico(palette="roma")

ggplot() +
  geom_rect(data=tk25_db, aes(xmin = XLU, xmax = XRU, 
                              ymin = YLU, ymax = YLO), fill=NA, colour="black") + 
  geom_point(data=tk25_db, aes(x = XQMITTE, y = YQMITTE, fill=KARTE_QUAD), size=1/5) 
  

# Polygons are intersecting, thus XQMITTE, YQMITTE are not in a regular distance interval (geom_tile does not work)
# => One could fix this by creating polygons for each grid cell, intersecting the polygons and calculating the mid point!

#' Turn quadrants into raster
#' 
(r_tk4tel <- raster::rasterFromXYZ(tk25_db[,c("XQMITTE","YQMITTE", "KARTE_QUAD")], 
                                   crs=sp::CRS("+init=epsg:31468"),
                                 res=c(6100, 5550), digits=0))
raster::plot(r_tk4tel)
length(unique(r_tk4tel[]))
tk4tel_grid <- as.data.frame(raster::rasterToPoints(r_tk4tel))
save(tk4tel_grid, file="data/tk4tel_grid.rda", compress = "xz")

#' Turn quadrants into raster
#' 
tk25_1 <- tk25_db %>% group_by(KARTE) %>% summarise_at(vars(XLU:YRU), min)
tk25_2 <- tk25_db %>% group_by(KARTE) %>% summarise_at(vars(XRO:YLO), max)
tk25_3 <- tk25_db %>% group_by(KARTE) %>% summarise_at(vars(XQMITTE:YQMITTE), mean)
tk25 <- left_join(tk25_1, tk25_2) %>% left_join(tk25_3)
head(tk25)

library(scico)
ggplot() + geom_tile(data=tk25, aes())
ggplot() +
  geom_rect(data=tk25, aes(xmin = XLU, xmax = XRO, 
                           ymin = YLU, ymax = YRO, fill=KARTE)) + 
  scale_fill_scico(palette="roma")

(r_tk25 <- raster::rasterFromXYZ(tk25[,c("XQMITTE","YQMITTE", "KARTE")], 
                                 crs=sp::CRS("+init=epsg:31468"),
                                 res=c(12200, 11100), digits=0))
raster::plot(r_tk25)

temp <- tempfile(fileext = ".zip")
utils::download.file("https://geo.dianacht.de/mtb/mtbshape.zip", destfile=temp, method = "auto", quiet = TRUE, mode="wb")
mtb_shape <- unzip(temp, exdir="Shapes")
unlink(temp)
mtb_shape <- sf::st_read(mtb_shape[[1]])
plot(st_geometry(mtb_shape))
library(raster)

st_crs(mtb_shape) <- "+proj=longlat +ellps=bessel +nadgrids=/usr/share/proj/BETA2007.gsb +no_defs"
unique(mtb_shape$LAND)
mtb_bav <- mtb_shape %>% filter(LAND %in% c("de,by,he,th", "de,by,th", "de,by,he", "de,by,sn,th", "de,by", "de,by,sn", "de,by,bw", "de,by,bw,he"))
length(unique(substring(mtb_bav$NAME,1,2))) # y
length(unique(substring(mtb_bav$NAME,3,4))) # x
mtb_bav$NAME <- as.numeric(mtb_bav$NAME)

r <- raster(ncol=31, nrow=34)
extent(r) <- extent(mtb_bav)
mtb_r <- rasterize(as(mtb_bav, "Spatial"), r, field="NAME")
mtb_gk <- projectRaster(mtb_r, crs=sp::CRS("+init=epsg:31468"), method="ngb")
mtb_gk <- crop(mtb_gk, r_tk25)
plot(mtb_gk)

tk25_grid <- as.data.frame(rasterToPoints(mtb_gk)) %>% arrange(layer,x,y) %>%
  rename(KARTE = layer)
save(tk25_grid, file="data/tk25_grid.rda", compress = "xz")

#df_mtb_gk_4tel <- as.data.frame(rasterToPoints(mtb_gk_4tel)) %>% arrange(layer,x,y) %>%
#  mutate(quadrant=rep(1:4, times=621)) %>% unite("karte_quad", layer:quadrant, sep="") %>%
#  filter(karte_quad %in% tk25_db$KARTE_QUAD)
#head(df_mtb_gk_4tel)
#mtb_gk_4tel <- rasterFromXYZ(df_mtb_gk_4tel)
#plot(mtb_gk_4tel)

# Turn TK25 into polygons
pls <- lapply(1:nrow(tk25), function(x){
  st_polygon(list(rbind(c(tk25[x,]$XLU, tk25[x,]$YLU), c(tk25[x,]$XRU,tk25[x,]$YRU), 
                        c(tk25[x,]$XRO, tk25[x,]$YRO), c(tk25[x,]$XLO,tk25[x,]$YLO),
                        c(tk25[x,]$XLU, tk25[x,]$YLU))))
})
pl <- st_multipolygon(pls)
plot(pl)
pl <- as(pl, "Spatial")
pl <- st_as_sf(pl)
st_crs(pl) <- "+init=epsg:31468"
