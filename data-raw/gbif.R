#' ## Extract GBIF data for Germany

# Load libraries
library(dplyr); library(raster); library(ggplot2)

# Get GADM boundaries
#country <- "DEU"
#gadm <- getData(name="GADM", country=country, level=0, path="H:/Data/GADM")

load("data/bavaria.rda")
gadm <- bavaria

# Connect to GBIF database
con <- DBI::dbConnect(RSQLite::SQLite(), dbname =paste0(filedir,"/gbif_database.sqlite"))
gbif <- tbl(con, "gbif")

# Collect data from GBIF database
minlat <- floor(min(gadm$lat))
maxlat <- ceiling(max(gadm$lat))
minlong <- floor(min(gadm$long))
maxlong <- ceiling(max(gadm$long))
data <- gbif %>% filter(decimallatitude >= minlat & decimallatitude <= maxlat) %>% 
  filter(decimallongitude >= minlong & decimallongitude <= maxlong) %>% 
  collect() %>% data.frame()

# Drop some variables
data <- dplyr::select(data, -c(datasetkey, occurrenceid, eventdate, depth, depthaccuracy, typestatus, issue))

# Write data to file
#write.csv(data, paste0("C:/Users/admin/Documents/Github/bdc/data/gbif_", country, ".csv"))

#gbif_deu <- data
#save(gbif_deu, file="data/gbif_deu.rda", compress="xz")

gbif_bav <- gbif_deu %>% filter(decimallatitude >= minlat & decimallatitude <= maxlat) %>% 
  filter(decimallongitude >= minlong & decimallongitude <= maxlong)

save(gbif_bav, file="data/gbif_bav.rda", compress="xz")

# Disconnect database
DBI::dbDisconnect(con); rm(gbif, con)
