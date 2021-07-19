#' ---
#' title: "Get Bavarian range maps from Excel into R"
#' author: "RS-eco"
#' ---

rm(list=ls())
library(tidyverse)

# Install bdc package for tk25_grid
if(!("bdc" %in% installed.packages()[,"Package"])){remotes::install_github("RS-eco/bdc")}

# Set working directory
filedir <- "/home/matt/t6p/group_hof/@BayKliF/data/"

# Define species names (have to correspond to sheet names in .ods file)
species_names <- c("Aeshna_subarctica", "Cordulegaster_bidentata", "Cordulegaster_boltonii",
                   "Cordulia_aenea", "Onychogomphus_forcipatus", "Ophiogomphus_cecilia",
                   "Orthetrum_cancellatum", "Sympetrum_depressiusculum", "Saxicola_rubetra", "Saxicola_torquatus")

# Read digitized file for one species
library(readODS)
odonata_tk4tel <- lapply(species_names, function(species){
  dat <- readODS::read_ods(path=paste0(filedir, "Libellen_ASK_2016_digitized.ods"), sheet=species)
  colnames(dat) <- gsub("NA", "", paste0(dat[c(1),], dat[c(2),]))
  dat <- dat[-c(1:3),-c(1:3)]
  row_names <- paste0(dat[,c("MTB")], dat[,c("Quadrant (oben = o, unten = u)")])[1:69]
  col_names <- colnames(dat)
  dat <- dat %>% select(`19r`:`48r`)
  dat <- dat[-c(68:71),] %>% select(`19r`:`48r`)
  rownames(dat) <- row_names[1:67]
  dat$rown <- row_names[1:67]
  head(dat)
  
  dat <- dat %>% gather(coln, presence, -c(rown))
  unique(dat$presence)
  dat$presence[dat$presence == "x"] <- 0
  dat$presence[dat$presence == ""] <- 0
  dat <- dat %>% drop_na()
  dat$presence <- sub("x", 1, dat$presence)
  head(dat)
  dat$K1 <- substr(dat$rown, 1, 2)
  dat$K2 <- substr(dat$rown, 3, 3)
  dat$K3 <- substr(dat$coln, 1, 2)
  dat$K4 <- substr(dat$coln, 3, 3)
  dat$KARTE <- paste0(dat$K1, dat$K3)
  dat$QUAD <- paste0(dat$K2, dat$K4)
  dat$QUAD <- factor(dat$QUAD, levels=c("ol", "or", "ul", "ur"), labels=c(1,2,3,4))
  dat$KARTE_QUAD <- as.numeric(paste0(dat$KARTE, dat$QUAD))
  
  # Get grid information
  library(bdc)
  data(tk4tel_grid)
  head(tk4tel_grid)
  
  dat <- left_join(dat, tk4tel_grid)
  dat$species <- species
  return(dat)
})
odonata_tk4tel <- bind_rows(odonata_tk4tel)

odonata_tk4tel %>% filter(species == "Cordulegaster_boltonii") %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=presence), na.color="grey")
odonata_tk4tel %>% filter(species == "Saxicola_rubetra") %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=presence), na.color="grey")

# => One cell has 0 value???

ggplot() + geom_tile(data=odonata_tk4tel, aes(x=x, y=y, fill=presence)) + 
  facet_wrap(.~species)

library(magrittr)
odonata_tk4tel %<>% select(c(x, y, KARTE_QUAD, species, presence))
save(odonata_tk25, file="data/odonata_tk4tel.rda", compress="xz")
