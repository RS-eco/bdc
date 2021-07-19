#' # Get taxon information

#' Read data
#data <- read.csv("data/artennamen.csv")

# Load database
library(dplyr)

# Load ASK database
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "./extdata/ASK.db")
# Pull part of ASK database including data on species 
ask_data <- dplyr::tbl(my_db, paste("ask_art"))

# Load background grid
tk25 <- dplyr::tbl(my_db, "geo_tk25_quadranten") %>% dplyr::collect() %>%
  tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="/", remove = TRUE) %>% 
  dplyr::select(-c("KARTE_QUAD"))

# Load taxonomy
load("./data/taxonomyStd.rda")

# Load ask_fuo
ask_fuo <- dplyr::tbl(my_db, "ask_fuo") %>% collect()

# combine gridded map from ask_art with full locations of recorded species and taxonomy
ask_art <- ask_data %>% dplyr::select(-c(gkk_rw, gkk_hw, objnr, rw, hw)) %>% collect() %>% 
  left_join(ask_fuo) %>% left_join(tk25) %>%
  left_join(taxonomyStd, by=c("art" = "scientificName"))
ask_art <- ask_art %>% drop_na(class, order)
ask_art$KARTE_QUAD <- as.numeric(sub("/", "", ask_art$quadrant))

# Load Ornitho database
dat_ornitho <- vroom::vroom("./extdata/dda-observations_bayern_2021-03-17.csv")
length(unique(ask_art$art))
length(unique(dat_ornitho$LATIN_SPECIES))
data_names <- data.frame(x=1:length(unique(c(ask_art$art, toupper(dat_ornitho$LATIN_SPECIES)))))
data_names$scientificName <- factor(unique(c(ask_art$art, toupper(dat_ornitho$LATIN_SPECIES))))

# Get standardised species names
source("R/standardize_taxonomy.R")
source("R/get_taxonomy.R")
taxonomyStd <- standardize_taxonomy(x=data_names, preferred_data_sources=c(3,4,11,12,118,138,163,173,174,175,179))
save(taxonomyStd, file="data/taxonomyStd.rda", compress="xz")

# Check non-standardised names
load("data/taxonomyStd.rda")
(z <- length(which(is.na(taxonomyStd$scientificNameStd))))
mis_names <- data.frame(x=c(1:z))
mis_names$wrongname <- taxonomyStd$scientificName[
  is.na(taxonomyStd$scientificNameStd)]

# Manually provide synonyms
synonyms <- data.frame(name=c("POLYOMMATUS EUMEDON", "SPATULA QUERQUEDULA", 
                              "CYANECULA SVECICA", "SPATULA CLYPEATA", 
                              "LEIOPICUS MEDIUS","MARECA STREPERA", 
                              "CALIDRIS MELANOTUS", "POLYOMMATUS ARTAXERXES", 
                              "ZAPORNIA PARVA", "ZAPORNIA PUSILLA", 
                              "POLYOMMATUS ORBITULUS", "GRYLLIDAE", 
                              "COLUMBA LIVIA FO. DOMESTICA", 
                              "INACHIS SPEC.", "TETRIGIDAE", "MOTACILLA THUNBERGI", 
                              "MICROCARBO PYGMAEUS", "MOTACILLA FELDEGG",
                              "KORSCHELTELLUS LUPULINA", "POLYOMMATUS AGESTIS",
                              "REGULUS IGNICAPILLUS", "EULEIOPTILUS DISTINCTUS",
                              "CEPHALISPHEIRA FERRUGELLA", "ACANTHOPHILA LATIPENNELLA",
                              "CYDIA CORONILLANA", "DAHLICA WOCKII",
                              "SCOLITANTIDES BATON", "POLYPHAENIS VIRIDIS",
                              "CEPHALISPHEIRA DENISELLA", "CYDIA LATHYRANA",
                              "EUGRAPHE SUBROSEA", "PHYLLONORYCTER KLEEMANNELLA",
                              "DIESTRAMMENA ASYNAMORUS", "ACANTHOPHILA ALACELLA",
                              "CHLIDONIAS HYBRIDUS", "CARCHARODUS FLOCCIFERUS",
                              "NOTHRIS LEMNISCELLA", "PLATYPTILIA CAPNODACTYLUS",
                              "ANTHEREA YAMAMAI", "EMMETIA SZOECSI"),
                       scientificName=c("Eumedonia eumedon", "Anas querquedula",
                                        "Luscinia svecica", "Anas clypeata",
                                        "Dendrocoptes medius", "Anas strepera",
                                        "Calidris melanotos", "Aricia artaxerxes",
                                        "Porzana parva", "Porzana pusilla",
                                        "Aricia orbitulus", "Gryllides",
                                        "Columba livia", "Inachis spec.",
                                        "Tetrigidae", "Motacilla flava thunbergi", 
                                        "Phalacrocorax", "Motacilla flava feldegg",
                                        "Pharmacis lupulina", "Aricia agestis",
                                        "Regulus ignicapilla", "Hellinsia distinctus",
                                        "Orophia ferrugella", "Dichomeris latipennella",
                                        "Grapholita coronillana", "Dahlica wockei",
                                        "Pseudophilotes baton", "Polyphaenis sericata",
                                        "Orophia denisella", "Grapholita lathyrana",
                                        "Coenophila subrosea", "Phyllonorycter klemannella",
                                        "Diestrammena asynamora", "Gelechia alacella",
                                        "Chlidonias hybrida", "Carcharodus floccifera",
                                        "Nothris lemniscellus", "Buszkoiana capnodactylus",
                                        "Antheraea yamamai", "Coptotriche szocsi"))

# Join names with synonyms
mis_names <- dplyr::left_join(mis_names, synonyms, by=c("wrongname"="name"))

# Get taxon information
taxon_misStd <- standardize_taxonomy(x=mis_names, 
                                     preferred_data_sources=c(3,4,11,12,118,138,163,
                                                              173,174,175,179))
taxon_mis <- taxon_misStd[is.na(taxon_misStd$scientificNameStd),]
taxon_misStd <- taxon_misStd[!is.na(taxon_misStd$scientificNameStd),]
library(magrittr)
taxon_misStd %<>% dplyr::mutate(scientificName = wrongname) %>% 
  dplyr::select(-wrongname)

# Add taxon information to file
taxonomyStd <- taxonomyStd[!is.na(taxonomyStd$scientificNameStd),]
taxonomyStd <- dplyr::bind_rows(taxonomyStd, taxon_misStd)

# Add remaining missing information manually
taxon_mis %>% arrange(wrongname)

#taxon_mis_orig <- sort(data_names$scientificName[which(!data_names$scientificName %in% taxonomyStd$scientificName)])
#taxon_mis_orig

taxon_mis2 <- data.frame(scientificNameStd = c("Gryllidae", "Inachis spec.", "Tetrigidae", "Pyralidae", "Gracillariidae", "Saturniidae"),
                         scientificName = c("GRYLLIDAE", "INACHIS SPEC.", "TETRIGIDAE", "PYRALIDAE", "GRACILLARIIDAE", "SATURNIIDAE"),
                         kingdom = rep("Animalia", 6), phylum =  rep("Arthropoda", 6),
                         class = rep("Insecta", 3), 
                         order = c("Orthoptera", "Lepidoptera", "Orthoptera", "Lepidoptera", "Lepidoptera", "Lepidoptera"), 
                         family = c("Gryllidae", "Nymphalidae", "Tetrigidae", "Pyralidae", "Gracillariidae", "Saturniidae"))

.simpleCap <- function(x) {
  paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)),
        sep = "", collapse = " ")
}

#taxon_mis3 <- data.frame(scientificNameStd =  c(sapply(as.character(sort(data_names$scientificName[which(!data_names$scientificName %in% taxonomyStd$scientificName)])), function(x) .simpleCap(x)),
#                                                "Scopula spec.", "Stigmella spec."),
#                         scientificName =  c(as.character(sort(data_names$scientificName[which(!data_names$scientificName %in% taxonomyStd$scientificName)])),
#                                             "SCOPULA SPEC.", "STIGMELLA SPEC."),
#                         kingdom = rep("Animalia", 26), 
#                         phylum =  c(rep("Arthropoda", 3), "Chordata", rep("Arthropoda", 3),
#                                     "Chordata", rep("Arthropoda", 14), "Chordata", rep("Arthropoda", 3)),
#                         class =  c(rep("Insecta", 3), "Aves", rep("Insecta", 3),
#                                    "Aves", rep("Insecta", 14), "Aves", rep("Insecta", 3)),
#                         order = c(rep("Lepidoptera", 3), "Charadriiformes", rep("Lepidoptera", 3),
#                                   "Charadriiformes", rep("Lepidoptera", 14), "Charadriiformes", rep("Lepidoptera", 3)),
#                         family = c("Gelechiidae", "Gelechiidae", "Saturniidae", "Scolopacidae", "Hesperiidae",
#                                    "Depressariidae", "Depressariidae", "Laridae", "Tortricidae", "Tortricidae", "Psychidae", 
#                                    "Rhaphidophoridae", "Tischeriidae", "Noctuidae", "Pterophoridae", "Hepialidae", "Gelechiidae", 
#                                    "Gracillariidae", "Pterophoridae", "Lycaenidae", "Lycaenidae", "Noctuidae", 
#                                    "Regulidae", "Lycaenidae", "Geometridae", "Nepticulidae"))

taxonomyStd <- bind_rows(taxonomyStd, taxon_mis2)
#taxonomyStd <- bind_rows(bind_rows(taxonomyStd, taxon_mis2), taxon_mis3)
taxonomyStd <- dplyr::select(taxonomyStd, -x)
save(taxonomyStd, file="data/taxonomyStd.rda", compress="xz")

load("data/taxonomyStd.rda")
taxonomyStd$family[taxonomyStd$scientificName == "TURDUS MERULA"] <- "Turdidae"
taxonomyStd$family[taxonomyStd$scientificName == "TURDUS ILIACUS"] <- "Turdidae"
taxonomyStd$family[taxonomyStd$scientificName == "TURDUS PHILOMELOS"] <- "Turdidae"
taxonomyStd$family[taxonomyStd$scientificName == "TURDUS PILARIS"] <- "Turdidae"
taxonomyStd$family[taxonomyStd$scientificName == "TURDUS TORQUATUS"] <- "Turdidae"
taxonomyStd$family[taxonomyStd$scientificName == "TURDUS VISCIVORUS"] <- "Turdidae"
taxonomyStd$family[taxonomyStd$scientificName == "TURDUS SP."] <- "Turdidae"

taxonomyStd$kingdom[taxonomyStd$scientificName == "PIERIS SPEC."] <- "Animalia"
taxonomyStd$phylum[taxonomyStd$scientificName == "PIERIS SPEC."] <- "Arthropoda"
taxonomyStd$class[taxonomyStd$scientificName == "PIERIS SPEC."] <- "Insecta"
taxonomyStd$order[taxonomyStd$scientificName == "PIERIS SPEC."] <- "Lepidoptera"
taxonomyStd$family[taxonomyStd$scientificName == "PIERIS SPEC."] <- "Pieridae"
save(taxonomyStd, file="data/taxonomyStd.rda", compress="xz")

taxonomyStd[taxonomyStd$scientificName == "PHYLLONORYCTER KLEEMANNELLA",]
taxonomyStd[taxonomyStd$scientificName == "DAHLICA WOCKII",]

# Add remaining missing information manually
(taxon_mis <- taxonomyStd[is.na(taxonomyStd$scientificNameStd),])
(taxon_mis <- taxonomyStd[is.na(taxonomyStd$kingdom),])
(taxon_mis <- taxonomyStd[is.na(taxonomyStd$phylum),])
(taxon_mis <- taxonomyStd[is.na(taxonomyStd$class),])
(taxon_mis <- taxonomyStd[is.na(taxonomyStd$order),])
(taxon_mis <- taxonomyStd[is.na(taxonomyStd$family),])
