# Altitude and terrain data

# SRTM 3ArcSec Data was downloaded from: https://gdex.cr.usgs.gov/gdex/
# https://search.earthdata.nasa.gov/

filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/SRTM"

file <- c(paste0(filedir, "/NASA_SRTM_v3.0_3ArcSec_Bav.tif"), 
          paste0(filedir, "/NGA_SRTM_3ArcSec_Bav.tif"))[1]

library(raster)
alt_bav <- raster(file)
names(alt_bav)

# Load bavaria outline
load("data/bavaria.rda")

# Turn to tk4tel grid
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
raster::projection(tk4tel_r) <- sp::CRS("+init=epsg:31468")
alt_bav <- raster::projectRaster(alt_bav, crs=sp::CRS("+init=epsg:31468"))

# Calculate terrain variables
slope <- terrain(alt_bav, opt='slope')
aspect <- terrain(alt_bav, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Bavaria')
plot(alt_bav, col=rainbow(25, alpha=0.35), add=TRUE)

# Terrain Ruggedness Index (TRI)
tri <- terrain(alt_bav, opt="TRI")
#TRI <- focal(x, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)

# Topographic Position Index (TPI)
tpi <- terrain(alt_bav, opt="TPI")
#TPI <- focal(x, w=f, fun=function(x, ...) x[5] - mean(x[-5]), pad=TRUE, padValue=NA)

# TPI for different neighborhood size:
tpiw <- function(x, w=5) {
  m <- matrix(1/(w^2-1), nc=w, nr=w)
  m[ceiling(0.5 * length(m))] <- 0
  f <- focal(x, m)
  x - f
}
tpi5 <- tpiw(alt_bav)

# Roughness
roughness <-terrain(alt_bav, opt="roughness")
#rough <- focal(x, w=f, fun=function(x, ...) max(x) - min(x), pad=TRUE, padValue=NA, na.rm=TRUE)

# flowdir
flowdir <- terrain(alt_bav, opt="flowdir")

# Stack together, resample and crop by extent
alt_bav_tk4tel <- stack(alt_bav, aspect, slope, hill, tri, tpi, tpi5, roughness, flowdir)
alt_bav_tk4tel <- raster::resample(alt_bav_tk4tel, tk4tel_r, method="bilinear")
alt_bav_tk4tel <- raster::mask(raster::crop(alt_bav_tk4tel, tk4tel_r), tk4tel_r)

# Turn into tbl_cube
alt_bav_tk4tel <- as.data.frame(rasterToPoints(alt_bav_tk4tel))
colnames(alt_bav_tk4tel) <- c("x", "y", "altitude", "aspect", "slope", "hillshade", "tri", "tpi", 
                                "tpi5", "roughness", "flowdir")

# Save to file
save(alt_bav_tk4tel, file="data/alt_bav_tk4tel.rda", compress="xz")
