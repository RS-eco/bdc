# Load libraries
library(dplyr); library(raster); library(ggplot2)

# Get GADM boundaries
country <- "DEU" # "DEU"
gadm <- getData(name="GADM", country=country, level=0, path="C:/Users/admin/Documents/Data/GADM")
gadm <- fortify(gadm)

# Set file directory
filedir <- "D:/"

# Download ebird data
#download.file(url="https://download.ebird.org/ebd/prepackaged/ebd_relJun-2020.tar", 
#              destfile="/home/matt/ebd_relJun-2020.tar")
              
# => Checkout auk package!!!
# see ebird script from Dynacom Project

# Connect to ebird database
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(filedir, "/ebird_database.sqlite"))
ebird <- tbl(con, "ebird")

# Collect data from gbif
minlat <- round(min(gadm$lat),1)
maxlat <-  round(max(gadm$lat),1)
minlong <-  round(min(gadm$long),1)
maxlong <-  round(max(gadm$long),1); rm(gadm)
data <- ebird %>% filter(LATITUDE >= minlat & LATITUDE <= maxlat) %>% 
  filter(LONGITUDE >= minlong & LONGITUDE <= maxlong) %>% 
  collect() %>% data.frame()

# Write data to file
if(nrow(data) > 0){write.csv(data, paste0("C:/Users/admin/Documents/Github/bdc/data/ebird_", country, ".csv"))}

# Disconnect database
DBI::dbDisconnect(con); rm(ebird, con)

# Load libraries
library(dplyr); library(raster); library(ggplot2)

# Get GADM boundaries
country <- "DEU" # "DEU"
gadm <- getData(name="GADM", country=country, level=0, path="C:/Users/admin/Documents/Data/GADM")
gadm <- fortify(gadm)

# Set file directory
filedir <- "D:/"

# Connect to ebird database
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(filedir, "/ebird_database.sqlite"))
ebird <- tbl(con, "ebird")

# Collect data from gbif
minlat <- round(min(gadm$lat),1)
maxlat <-  round(max(gadm$lat),1)
minlong <-  round(min(gadm$long),1)
maxlong <-  round(max(gadm$long),1); rm(gadm)
data <- ebird %>% filter(LATITUDE >= minlat & LATITUDE <= maxlat) %>% 
  filter(LONGITUDE >= minlong & LONGITUDE <= maxlong) %>% 
  collect() %>% data.frame()

# Write data to file
if(nrow(data) > 0){write.csv(data, paste0("C:/Users/admin/Documents/Github/bdc/data/ebird_", country, ".csv"))}

# Disconnect database
DBI::dbDisconnect(con); rm(ebird, con)
