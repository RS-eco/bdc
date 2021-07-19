## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  results="asis", cache=TRUE,
  echo=F, warning=F, message=F,
  fig.width=10, fig.height=8
)

## ----load_pkgs----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(zoo)

# Define standard colour scheme
bluewhitered <- colorRampPalette(c("#009392","#39b185","#9ccb86","#e9e29c","#eeb479","#e88471","#cf597e"))(255)
#pie(rep(1,7),col=c("#009392","#39b185","#9ccb86","#e9e29c","#eeb479","#e88471","#cf597e"))

## -----------------------------------------------------------------------------
# Load shapefile of Bavaria
data("bavaria", package="bdc")
bavaria <- sf::st_as_sf(bavaria)

## -----------------------------------------------------------------------------
data("corine_lc_bav_tk4tel", package="bdc")
#head(corine_lc_bav_tk4tel)
colnames(corine_lc_bav_tk4tel) <- c("x", "y", "1990", "2000", "2006", "2012", "2018")

# Plot individual variable from long format
corine_lc_bav_tk4tel %>% ggplot() + 
  geom_tile(aes (x=x, y=y, fill=`1990`)) +
  scale_fill_manual(name="CLC 1990", values=c("#e6004d", "#ff0000", "#ffffa8", "#f2a64d", "#e6e64d",
                                               "#ffe64d", "#80ff00", "#00a600", "#4dff00", "#ccf24d",
                                               "#a6ff80", "#cccccc", "#a6a6ff", "#80f2e6")) + 
  coord_sf() + theme_bw()

corine_lc_bav_tk4tel %>% ggplot() + 
  geom_tile(aes (x=x, y=y, fill=`2000`)) + coord_sf() + theme_bw() + 
  scale_fill_manual(name="CLC 2000", values=c("#e6004d", "#ff0000", "#ffffa8", "#f2a64d", "#e6e64d",
                                               "#ffe64d", "#80ff00", "#00a600", "#4dff00", "#ccf24d",
                                               "#a6ff80", "#a6f200", "#cccccc", "#a6a6ff", "#80f2e6")) 

corine_lc_bav_tk4tel %>% ggplot() + geom_tile(aes (x=x, y=y, fill=`2006`)) +
  scale_fill_manual(name="CLC 2006",values=c("#e6004d", "#ff0000", "#ffffa8", "#f2a64d", "#e6e64d",
                                               "#ffe64d", "#80ff00", "#00a600", "#4dff00", "#ccf24d",
                                               "#a6ff80", "#a6f200", "#cccccc", "#a6a6ff", "#80f2e6")) + 
  coord_sf() + theme_bw()

corine_lc_bav_tk4tel %>% ggplot() + geom_tile(aes (x=x, y=y, fill=`2012`)) +
  scale_fill_manual(name="CLC 2012", values=c("#e6004d", "#ff0000", "#ffffa8", "#f2a64d", "#e6e64d",
                                               "#80ff00", "#00a600", "#4dff00", "#ccf24d",
                                               "#a6ff80", "#a6f200", "#cccccc", "#4d4dff", "#80f2e6")) + 
  coord_sf() + theme_bw()

corine_lc_bav_tk4tel %>% ggplot() + geom_tile(aes (x=x, y=y, fill=`2018`)) +
  scale_fill_manual(name="CLC 2018", values=c("#e6004d", "#ff0000", "#ffffa8", "#f2a64d", "#e6e64d",
                                               "#80ff00", "#00a600", "#4dff00", "#ccf24d",
                                               "#a6ff80", "#a6f200", "#cccccc", "#4d4dff", "#80f2e6")) + 
  coord_sf() + theme_bw()

