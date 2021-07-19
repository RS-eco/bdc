#' ## Extract biotime data for Bavaria

# Available for download from here: https://zenodo.org/record/3265871

#' Please cite as detailed below when using BioTIME:

#' Dornelas M, Antão LH, Moyes F, Bates, AE, Magurran, AE, et al. BioTIME: 
#' A database of biodiversity time series for the Anthropocene. 
#' Global Ecol Biogeogr. 2018; 27:760 - 786. https://doi.org/10.1111/geb.12729

filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/BioTime/"

biotime <-read.csv(paste0(filedir, "BioTIMEQuery02_04_2018.csv")) #### use the latest version from download site ####

#' Alternatively, you can load the query from the ZENODO repository

#fullquery<-read.csv(url(https://zenodo.org/record/1095627))


# Subset locations by extent of Germany (no data for Bavaria)
library(dplyr)
colnames(biotime)
biotime_deu <- biotime %>% filter(LONGITUDE >= 5 & LONGITUDE <= 16) %>% filter(LATITUDE >= 47 & LATITUDE <= 56)
head(biotime_deu)

save(biotime_deu, file="data/biotime_deu.rda", compress="xz")

#' Get GADM data of Germany
deu <- raster::getData("GADM", country="DEU", level=1, path="/media/matt/Data/Documents/Wissenschaft/Data/GADM")

#' Plot locations of data entries
library(sp)
coordinates(biotime_deu) <- ~LONGITUDE+LATITUDE
plot(deu)
plot(biotime_deu, add=T)
