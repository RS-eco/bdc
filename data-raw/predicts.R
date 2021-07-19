#' ## Predicts Database for Bavaria

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/"

# Load predicts data
predicts_data <- readRDS(paste0(filedir, "/PredictsDatabase/database.rds"))
predicts_sites <- readRDS(paste0(filedir, "/PredictsDatabase/sites.rds"))
#predicts_resources <- read.csv(paste0(filedir, "/PredictsDatabase/resource.csv"))
predicts_references <- read.csv(paste0(filedir, "/PredictsDatabase/references.csv"))

# Diversity metrics
levels(predicts_data$Diversity_metric)

# Use intensity
levels(predicts_data$Use_intensity)

# Pre-dominant land-use
levels(predicts_data$Predominant_land_use)

# Get column names
colnames(predicts_data)
head(predicts_data)

# Subset predicts sites by extent of Germany (no data for Bavaria)
library(dplyr)
predicts_deu <- predicts_sites %>% filter(Longitude >= 5 & Longitude <= 16) %>% filter(Latitude >= 47 & Latitude <= 56)
head(predicts_deu[,c("Latitude", "Longitude")])

#' Get GADM data of Germany
#deu <- raster::getData("GADM", country="DEU", level=1, path="/media/matt/Data/Documents/Wissenschaft/Data/GADM")

#library(sp)
#coordinates(predicts_deu) <- ~Longitude+Latitude
#plot(deu)
#plot(predicts_deu, add=T)

predicts_deu <- dplyr::left_join(predicts_deu, predicts_data) %>% left_join(predicts_references)
colnames(predicts_deu)

save(predicts_deu, file="data/predicts_deu.rda", compress="xz")
