## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = T, comment = NA, warning=F, message=F, eval=T, echo=T, error=F, 
  comment = "#>", dpi=100, fig.width=8, fig.height=6
)

## ----load_data----------------------------------------------------------------
# Load packages
library(dplyr); library(magrittr); library(scico)
library(lubridate); library(ggplot2)

# Load Ornitho database
dat <- vroom::vroom("../extdata/dda-observations_bayern_2021-03-17.csv")
#colnames(dat)

# Only select needed columns
dat %<>% select(c("LATIN_SPECIES", "FAMILY_NAME", "DATE", "TIMING", "PLACE", "COORD_LAT", "COORD_LON",
                  "COORD_F", "COORD_E", "COORD_N", "PRECISION", "TOTAL_COUNT", "ATLAS_CODE")); invisible(gc())

## -----------------------------------------------------------------------------
# Summarise number of records per year
dat$year <- year(dat$DATE)
dat %>% group_by(year) %>% summarise(n=n()) %>%
  ggplot() + geom_histogram(aes(x=year, y=n), stat="identity") + theme_bw() + 
  scale_x_continuous(name="Year", expand=c(0,1)) + 
  scale_y_continuous(name="Number of records", limits=c(0,NA), expand=c(0,10000))

## -----------------------------------------------------------------------------
dat %>% group_by(year) %>% filter(year %in% c(1950:2010)) %>% 
  summarise(n=n()) %>%
  ggplot() + geom_histogram(aes(x=year, y=n), stat="identity") + theme_bw() + 
  scale_x_continuous(name="Year (1950 - 2010)", expand=c(0,1)) + 
  scale_y_continuous(name="Number of records", #limits=c(0,7000), 
                     expand=c(0,0))

## -----------------------------------------------------------------------------
dat %>% group_by(year) %>% filter(year %in% c(2009:2021)) %>% 
  summarise(n=n()) %>%
  ggplot() + geom_histogram(aes(x=year, y=n), stat="identity") + theme_bw() + 
  scale_x_continuous(name="Year (2009 - 2021)", expand=c(0,0)) + 
  scale_y_continuous(name="Number of records", limits=c(0,12e5), expand=c(0,0))

## -----------------------------------------------------------------------------
# Summarise number of records per month
dat$month <- month(dat$DATE)
dat %>% group_by(month) %>% summarise(n=n()) %>%
  mutate(month = factor(month, levels=c(1:12), labels=month.abb)) %>%
  ggplot() + geom_histogram(aes(x=month, y=n), stat="identity") + 
  theme_bw() + 
  labs(x="Month") + scale_y_continuous(name="Number of records", limits=c(0,NA), expand=c(0,5000))

## -----------------------------------------------------------------------------
dat %>% filter(year %in% c(2001:2021)) %>% 
  group_by(month, year) %>% summarise(n=n()) %>%
  mutate(month = factor(month, levels=c(1:12), labels=month.abb)) %>%
  mutate(year = factor(year)) %>%
  ggplot() + geom_histogram(aes(x=month, y=n, fill=year), stat="identity", position="stack") + 
  theme_bw() + 
  labs(x="Month") + scale_y_continuous(name="Number of records", limits=c(0,NA), expand=c(0,5000))

## -----------------------------------------------------------------------------
# Summarise number of records per family
dat %>% group_by(FAMILY_NAME) %>% summarise(n=n()) %>%
  ggplot() + geom_histogram(aes(x=FAMILY_NAME, y=n), stat="identity") + 
  theme_bw() + labs(x="Family") + 
  scale_y_continuous(name="Number of records", limits=c(0,NA)) + 
  theme(axis.text.x = element_text(angle=90))

## -----------------------------------------------------------------------------
# Summarise number of records per location
dat %>% ungroup() %>% mutate(COORD_LON = round(COORD_LON, 2),
               COORD_LAT = round(COORD_LAT, 2)) %>%
  group_by(COORD_LON, COORD_LAT) %>% summarise(n=n()) %>%
  ggplot() + geom_point(aes(x=COORD_LON, y=COORD_LAT, colour=log10(n)), size=1) +
  scale_colour_scico(palette="roma") + coord_sf() + theme_bw()
rm(list=ls()); invisible(gc())

