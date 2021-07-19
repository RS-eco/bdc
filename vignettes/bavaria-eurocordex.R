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
library(ggspatial)
library(ggsci)
library(ggthemes)
library(patchwork)
library(zoo)

# Define standard colour scheme
whitered <- colorRampPalette(c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'))(255)
#pie(rep(1,9),col=c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'))

whiteblue <- colorRampPalette(c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081'))(255)
#pie(rep(1,9),col=c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081'))

bluewhitered <- colorRampPalette(c("#009392","#39b185","#9ccb86","#e9e29c","#eeb479","#e88471","#cf597e"))(255)
#pie(rep(1,7),col=c("#009392","#39b185","#9ccb86","#e9e29c","#eeb479","#e88471","#cf597e"))

## -----------------------------------------------------------------------------
# Load shapefile of Bavaria
data("bavaria", package="bdc")
bavaria <- sf::st_as_sf(bavaria)

## ---- eval=F------------------------------------------------------------------
#  # Load data
#  data("cordex_tasAdjust_bav", package="bdc")
#  
#  # Calculate mean temperature of Bavaria for a given time period and scenario (gcm, rcp, rcm, ensemble)
#  mean_tasAdjust <- cordex_tasAdjust_bav %>% mutate(mon=lubridate::month(time), yr=lubridate::year(time)) %>%
#    group_by(mon, yr, gcm, rcp, rcm, ensemble) %>%
#    summarise(mn=mean(value, na.rm=T), err=sd(value, na.rm=T)); rm(cordex_tasAdjust_bav); invisible(gc())

## ---- eval=F, fig.width=10, fig.height=6--------------------------------------
#  mean_tasAdjust %>% filter(yr %in% c(1970, 2000, 2030, 2060, 2090)) %>%
#    group_by(mon, yr, rcp) %>% summarise(mn=mean(mn)) %>%
#    ggplot(aes(x=mon, y=mn, colour=factor(yr))) + facet_wrap(rcp~., nrow=1) +
#    geom_line() + scale_x_continuous(breaks=1:12, labels=month.abb) +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) +
#    theme_few() + theme(axis.text.x=element_text(angle=90)) + labs(x="", y="Mean monthly temperature")

## ---- eval=F, fig.width=10, fig.height=6--------------------------------------
#  mean_tasAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(zoo::as.yearmon(monyear))+14) %>%
#    group_by(monyear, rcp) %>% summarise(mn=mean(mn, na.rm=T), err=mean(err)) %>%
#    ggplot() + geom_ribbon(aes(x=monyear, ymin=mn-err, ymax=mn+err)) +
#    geom_line(aes(x=monyear, y=mn), colour="red") +
#    geom_smooth(aes(x=monyear, y=mn), method="lm") +
#    facet_wrap(rcp~., ncol=1, strip.position = "left") + scale_x_date(expand=c(.01,.01)) + #expand_scale(mult=c(0,.01))) +
#    ggthemes::theme_few() + theme(strip.placement = "outside") + labs(x="Year", y="")

## ---- eval=F, fig.width=10, fig.height=6--------------------------------------
#  mean_tasAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    group_by(yr, rcp, gcm, rcm, ensemble) %>%
#    summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(yr, rcp) %>%
#    summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T)) %>%
#    ggplot() + geom_ribbon(aes(x=yr, ymin=mn-err, ymax=mn+err), alpha=0.5) +
#    geom_line(aes(x=yr, y=mn)) + scale_x_continuous(expand=c(.01,.01)) +
#    facet_wrap(rcp~., ncol=1, strip.position = "left") +
#    geom_smooth(aes(x=yr, y=mn), method="lm") +
#    theme_few() + theme(strip.placement = "outside", legend.position="none") + labs(x="Year", y="")

## ---- eval=F------------------------------------------------------------------
#  mean_tasAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(as.yearmon(monyear))+14) %>%
#    group_by(monyear, rcp, gcm, rcm, ensemble) %>% summarise(mn2=mean(mn, na.rm=T), err=sd(mn)) %>%
#    ggplot() + geom_ribbon(aes(x=monyear, ymin=mn2-err, ymax=mn2+err)) +
#    geom_line(aes(x=monyear, y=mn2), colour="red") +
#    facet_grid(rcp ~ rcm+gcm+ensemble) + geom_smooth(aes(x=monyear, y=mn2), method="lm") +
#    scale_x_date(expand=c(.01,.01)) + theme_few() +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00"))

## ---- eval=F, fig.width=10, fig.height=9--------------------------------------
#  mean_tasAdjust %>% filter(yr %in% c(1970, 2000, 2030, 2060, 2090)) %>%
#    ggplot(aes(x=mon, y=mn, colour=factor(yr), linetype=rcp)) +
#    geom_line() + facet_wrap(. ~ rcm+gcm+ensemble, ncol=5) +
#    scale_x_continuous(breaks=1:12, labels=month.abb) +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) +
#    theme_few() + labs(x="", y="Mean monthly temperature") +
#    theme(legend.position=c(0.9,0.1), axis.text.x=element_text(angle=90))

## ---- eval=F------------------------------------------------------------------
#  mean_tasAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(zoo::as.yearmon(monyear))+14) %>%
#    ggplot() + geom_ribbon(aes(x=monyear, ymin=mn-err, ymax=mn+err)) +
#    geom_line(aes(x=monyear, y=mn, linetype=rcp, colour=rcp)) +
#    geom_smooth(aes(x=monyear, y=mn), method="lm") +
#    facet_wrap(. ~ rcm+gcm+ensemble, ncol=5) + scale_x_date(expand=c(.01,.01)) +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) +
#    theme_few() + labs(x="", y="Mean monthly temperature") + theme(legend.position=c(0.8,0.1))

## ---- eval=F, fig.width=10, fig.height=10-------------------------------------
#  mean_tasAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    group_by(yr, rcm, gcm, rcp, ensemble) %>% summarise(mn=mean(mn, na.rm=T), err=mean(err)) %>%
#    ggplot() + #geom_ribbon(aes(x=yr, ymin=mn-err, ymax=mn+err)) +
#    geom_line(aes(x=yr, y=mn, colour=rcp)) +
#    #geom_smooth(aes(x=yr, y=mn), method="lm") +
#    facet_wrap(. ~ rcm+gcm+ensemble, ncol=4) + #scale_x_date(expand=c(.01,.01)) +
#    scale_colour_manual(name="RCP", values=c("#0099B4FF", "#ADB6B6FF", "#AD002AFF")) +
#    theme_few() + labs(x="", y="Mean annual temperature") + theme(legend.position=c(0.8,0.1), panel.spacing.x = unit(0.75, "lines"))

## -----------------------------------------------------------------------------
# Load data
data("cordex_tasminAdjust_bav", package="bdc")

# Calculate mean temperature of Bavaria for a given time period and scenario (gcm, rcp, rcm, ensemble)
mean_tasminAdjust <- cordex_tasminAdjust_bav %>% mutate(mon=month(time), yr=year(time)) %>%
  group_by(mon, yr, gcm, rcp, rcm, ensemble) %>% 
  summarise(mn=mean(value, na.rm=T), err=sd(value, na.rm=T)); rm(cordex_tasminAdjust_bav); invisible(gc())

## ---- fig.width=10, fig.height=6----------------------------------------------
mean_tasminAdjust %>% filter(yr %in% c(1970, 2000, 2030, 2060, 2090)) %>% 
  group_by(mon, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  ggplot(aes(x=mon, y=mn, colour=factor(yr))) + facet_wrap(rcp~., nrow=1) + 
  geom_line() + scale_x_continuous(breaks=1:12, labels=month.abb) + 
  scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) + 
  theme_few() + theme(axis.text.x=element_text(angle=90)) + labs(x="", y="Mean monthly temperature")

## ---- fig.width=10, fig.height=6----------------------------------------------
mean_tasminAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
  mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(as.yearmon(monyear))+14) %>%
  group_by(monyear, rcp) %>% summarise(mn=mean(mn, na.rm=T), err=mean(err)) %>% 
  ggplot() + geom_ribbon(aes(x=monyear, ymin=mn-err, ymax=mn+err)) + 
  geom_line(aes(x=monyear, y=mn), colour="red") + 
  geom_smooth(aes(x=monyear, y=mn), method="lm") +
  facet_wrap(rcp~., ncol=1, strip.position = "left") + scale_x_date(expand=c(.01,.01)) + theme_few() + 
  theme(strip.placement = "outside") + labs(x="Year", y="")

## ---- fig.width=10, fig.height=6----------------------------------------------
mean_tasminAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
  group_by(yr, rcp, gcm, rcm, ensemble) %>% 
  summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(yr, rcp) %>%
  summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T)) %>%
  ggplot() + geom_ribbon(aes(x=yr, ymin=mn-err, ymax=mn+err), alpha=0.5) + 
  geom_line(aes(x=yr, y=mn)) + 
  facet_wrap(rcp~., ncol=1, strip.position = "left") + 
  geom_smooth(aes(x=yr, y=mn), method="lm") + scale_x_continuous(expand=c(.01,.01)) + 
  theme_few() + theme(strip.placement = "outside", legend.position="none") + labs(x="Year", y="")

## ---- eval=F------------------------------------------------------------------
#  mean_tasminAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(as.yearmon(monyear))+14) %>%
#    group_by(monyear, rcp, gcm, rcm, ensemble) %>% summarise(mn2=mean(mn, na.rm=T), err=sd(mn)) %>%
#    ggplot() + geom_ribbon(aes(x=monyear, ymin=mn2-err, ymax=mn2+err)) +
#    geom_line(aes(x=monyear, y=mn2), colour="red") +
#    facet_grid(rcp ~ rcm+gcm+ensemble) + geom_smooth(aes(x=monyear, y=mn2), method="lm") +
#    scale_x_date(expand=c(.01,.01)) + theme_few() +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00"))

## ---- fig.width=10, fig.height=9----------------------------------------------
mean_tasminAdjust %>% filter(yr %in% c(1970, 2000, 2030, 2060, 2090)) %>% 
  ggplot(aes(x=mon, y=mn, colour=factor(yr), linetype=rcp)) +
  geom_line() + facet_wrap(. ~ rcm+gcm+ensemble, ncol=5) + 
  scale_x_continuous(breaks=1:12, labels=month.abb) + 
  scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) + 
  theme_few() + labs(x="", y="Mean monthly temperature") + 
  theme(legend.position=c(0.9,0.1), axis.text.x=element_text(angle=90))

## ---- eval=F------------------------------------------------------------------
#  mean_tasminAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(zoo::as.yearmon(monyear))+14) %>%
#    ggplot() + geom_ribbon(aes(x=monyear, ymin=mn-err, ymax=mn+err)) +
#    geom_line(aes(x=monyear, y=mn, linetype=rcp, colour=rcp)) +
#    geom_smooth(aes(x=monyear, y=mn), method="lm") +
#    facet_wrap(. ~ rcm+gcm+ensemble, ncol=4) + scale_x_date(expand=c(.01,.01)) +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) +
#    theme_few() + labs(x="", y="Mean monthly temperature") + theme(legend.position="bottom")

## ---- fig.width=10, fig.height=10---------------------------------------------
mean_tasminAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
  group_by(yr, rcm, gcm, rcp, ensemble) %>% summarise(mn=mean(mn, na.rm=T), err=mean(err)) %>% 
  ggplot() + #geom_ribbon(aes(x=yr, ymin=mn-err, ymax=mn+err)) + 
  geom_line(aes(x=yr, y=mn, colour=rcp)) + 
  #geom_smooth(aes(x=yr, y=mn), method="lm") + 
  facet_wrap(. ~ rcm+gcm+ensemble, ncol=4) + #scale_x_date(expand=c(.01,.01)) + 
  scale_colour_manual(name="RCP", values=c("#0099B4FF", "#ADB6B6FF", "#AD002AFF")) + 
  theme_few() + labs(x="", y="Mean annual minimum temperature") + 
  theme(legend.position=c(0.8,0.1), panel.spacing.x = unit(0.75, "lines"))

## -----------------------------------------------------------------------------
# Load data
data("cordex_tasmaxAdjust_bav", package="bdc")

# Calculate mean temperature of Bavaria for a given time period and scenario (gcm, rcp, rcm, ensemble)
mean_tasmaxAdjust <- cordex_tasmaxAdjust_bav %>% mutate(mon=month(time), yr=year(time)) %>%
  group_by(mon, yr, gcm, rcp, rcm, ensemble) %>% 
  summarise(mn=mean(value, na.rm=T), err=sd(value, na.rm=T)); rm(cordex_tasmaxAdjust_bav); invisible(gc())

## ---- fig.width=10, fig.height=6----------------------------------------------
mean_tasmaxAdjust %>% filter(yr %in% c(1970, 2000, 2030, 2060, 2090)) %>% 
  group_by(mon, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  ggplot(aes(x=mon, y=mn, colour=factor(yr))) + facet_wrap(rcp~., nrow=1) + 
  geom_line() + scale_x_continuous(breaks=1:12, labels=month.abb) + 
  scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) + 
  theme_few() + theme(axis.text.x=element_text(angle=90)) + labs(x="", y="Mean monthly temperature")

## ---- fig.width=10, fig.height=6----------------------------------------------
mean_tasmaxAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
  mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(as.yearmon(monyear))+14) %>%
  group_by(monyear, rcp) %>% summarise(mn=mean(mn, na.rm=T), err=mean(err)) %>% 
  ggplot() + geom_ribbon(aes(x=monyear, ymin=mn-err, ymax=mn+err)) + 
  geom_line(aes(x=monyear, y=mn), colour="red") + 
  geom_smooth(aes(x=monyear, y=mn), method="lm") +
  facet_wrap(rcp~., ncol=1, strip.position = "left") + scale_x_date(expand=c(.01,.01)) + theme_few() + 
  theme(strip.placement = "outside") + labs(x="Year", y="")

## ---- fig.width=10, fig.height=6----------------------------------------------
mean_tasmaxAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
  group_by(yr, rcp, gcm, rcm, ensemble) %>% 
  summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(yr, rcp) %>%
  summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T)) %>%
  ggplot() + geom_ribbon(aes(x=yr, ymin=mn-err, ymax=mn+err), alpha=0.5) + 
  geom_line(aes(x=yr, y=mn)) + 
  facet_wrap(rcp~., ncol=1, strip.position = "left") + 
  geom_smooth(aes(x=yr, y=mn), method="lm") + scale_x_continuous(expand=c(.01,.01)) + 
  theme_few() + theme(strip.placement = "outside", legend.position="none") + labs(x="Year", y="")

## ---- eval=F------------------------------------------------------------------
#  mean_tasmaxAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(as.yearmon(monyear))+14) %>%
#    group_by(monyear, rcp, gcm, rcm, ensemble) %>% summarise(mn2=mean(mn, na.rm=T), err=sd(mn)) %>%
#    ggplot() + geom_ribbon(aes(x=monyear, ymin=mn2-err, ymax=mn2+err)) +
#    geom_line(aes(x=monyear, y=mn2), colour="red") +
#    facet_grid(rcp ~ rcm+gcm+ensemble) + geom_smooth(aes(x=monyear, y=mn2), method="lm") +
#    scale_x_date(expand=c(.01,.01)) + theme_few() +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00"))

## ---- fig.width=10, fig.height=9----------------------------------------------
mean_tasmaxAdjust %>% filter(yr %in% c(1970,2000, 2030, 2060, 2090)) %>% 
  ggplot(aes(x=mon, y=mn, colour=factor(yr), linetype=rcp)) +
  geom_line() + facet_wrap(. ~ rcm+gcm+ensemble, ncol=5) + 
  scale_x_continuous(breaks=1:12, labels=month.abb) + 
  scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) + 
  theme_few() + labs(x="", y="Mean monthly maximum temperature") + 
  theme(legend.position=c(0.9,0.1), axis.text.x=element_text(angle=90))

## ---- eval=F------------------------------------------------------------------
#  mean_tasmaxAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(zoo::as.yearmon(monyear))+14) %>%
#    ggplot() + geom_ribbon(aes(x=monyear, ymin=mn-err, ymax=mn+err)) +
#    geom_line(aes(x=monyear, y=mn, linetype=rcp, colour=rcp)) +
#    geom_smooth(aes(x=monyear, y=mn), method="lm") +
#    facet_wrap(. ~ rcm+gcm+ensemble, ncol=5) + scale_x_date(expand=c(.01,.01)) +
#    scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) +
#    theme_few() + labs(x="", y="Mean monthly temperature") +
#    theme(legend.position="bottom")

## ---- fig.width=10, fig.height=10---------------------------------------------
mean_tasmaxAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
  group_by(yr, rcm, gcm, rcp, ensemble) %>% summarise(mn=mean(mn, na.rm=T), err=mean(err)) %>% 
  ggplot() + #geom_ribbon(aes(x=yr, ymin=mn-err, ymax=mn+err)) + 
  geom_line(aes(x=yr, y=mn, colour=rcp)) + 
  #geom_smooth(aes(x=yr, y=mn), method="lm") + 
  facet_wrap(. ~ rcm+gcm+ensemble, ncol=4) + #scale_x_date(expand=c(.01,.01)) + 
  scale_colour_manual(name="RCP", values=c("#0099B4FF", "#ADB6B6FF", "#AD002AFF")) + 
  theme_few() + labs(x="", y="Mean annual maximum temperature") + 
  theme(legend.position=c(0.8,0.1), panel.spacing.x = unit(0.75, "lines"))

## ---- fig.width=10, fig.height=6----------------------------------------------
data("cordex_prAdjust_bav", package="bdc")
mn_prAdjust <- cordex_prAdjust_bav %>% mutate(mon=month(time), yr=year(time)) %>%
  group_by(mon, yr, gcm, rcp, rcm, ensemble) %>% 
  summarise(mn=mean(value), err=sd(value)); rm(cordex_prAdjust_bav); invisible(gc())

mn_prAdjust %>% filter(yr %in% c(1970, 2000, 2030, 2060, 2090)) %>% 
  group_by(mon, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  ggplot(aes(x=mon, y=mn, colour=factor(yr))) + facet_wrap(rcp~., nrow=1) + 
  geom_line() + scale_x_continuous(breaks=1:12, labels=month.abb) + 
  scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) + 
  theme_few() + theme(axis.text.x=element_text(angle=90)) + labs(x="", y="Mean monthly precipitation (mm)")

## ---- fig.width=10, fig.height=6----------------------------------------------
mn_prAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
  mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(zoo::as.yearmon(monyear))+14) %>%
  group_by(monyear, rcp) %>% summarise(mn=mean(mn), err=sd(mn)) %>%
  ggplot() + geom_ribbon(aes(x=monyear, ymin=mn-err, ymax=mn+err), colour="black") + 
  geom_line(aes(x=monyear, y=mn), colour="red") + 
  geom_smooth(aes(x=monyear, y=mn), method="lm") + scale_x_date(expand=c(.01,.01)) + 
  facet_wrap(.~rcp, ncol=1, strip.position="left") + theme_few() + 
  theme(strip.placement = "outside") + labs(x="Year", y="")

## ---- fig.width=10, fig.height=6----------------------------------------------
mn_prAdjust %>% ungroup() %>% group_by(yr, rcp, gcm, rcm, ensemble) %>% 
  summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(yr, rcp) %>%
  summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T)) %>%
  ggplot() + geom_ribbon(aes(x=yr, ymin=mn-err, ymax=mn+err), alpha=0.5) + 
  geom_line(aes(x=yr, y=mn)) + 
  facet_wrap(rcp~., ncol=1, strip.position = "left") + 
  geom_smooth(aes(x=yr, y=mn), method="lm") + scale_x_continuous(expand=c(.01,.01)) + 
  theme_few() + theme(strip.placement = "outside", legend.position="none") + labs(x="Year", y="")

## ---- fig.width=10, fig.height=9----------------------------------------------
mn_prAdjust %>% filter(yr %in% c(1970, 2000, 2030, 2060, 2090)) %>% 
  ggplot(aes(x=mon, y=mn, colour=factor(yr), linetype=rcp)) +
  geom_line() + facet_wrap(. ~ rcm+gcm, ncol=5) + 
  scale_x_continuous(breaks=1:12, labels=month.abb) + 
  scale_colour_manual(name="Year", values=c('#e41a1c','#377eb8','#4daf4a',"#984ea3","#ff7f00")) + 
  theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1), axis.text.x=element_text(angle=90))

## ---- eval=F------------------------------------------------------------------
#  mn_prAdjust %>% ungroup() %>% mutate(mon = (mon-1)/12) %>%
#    mutate(monyear=yr + mon) %>% mutate(monyear = as.Date(zoo::as.yearmon(monyear))+14) %>%
#    ggplot() + geom_line(aes(x=monyear, y=mn, colour=rcp)) +
#    geom_smooth(aes(x=monyear, y=mn), method="lm") +
#    facet_wrap(. ~ rcm+gcm, ncol=5) + scale_x_date(expand=c(.01,.01)) +
#    theme_few() + theme(legend.position="bottom") + labs(x="", y="Mean monthly precipitation (mm)")

## ---- fig.width=10, fig.height=10---------------------------------------------
mn_prAdjust %>% group_by(yr, rcm, gcm, rcp, ensemble) %>% 
  summarise(tot=sum(mn)) %>% 
  ggplot() + geom_line(aes(x=yr, y=tot, colour=rcp)) + 
  facet_wrap(. ~ rcm+gcm+ensemble, ncol=4) + 
  scale_colour_manual(name="RCP", values=c("#0099B4FF", "#ADB6B6FF", "#AD002AFF")) + 
  theme_few() + labs(x="", y="Mean annual precipitation (mm)") + 
  theme(legend.position=c(0.8,0.1), panel.spacing.x = unit(0.75, "lines"))

## ---- eval=F, fig.width=10, fig.height=8--------------------------------------
#  data("cordex_tasAdjust_bav", package="bdc")
#  
#  tasAdjust_30yr <- cordex_tasAdjust_bav %>%
#    mutate(yr = lubridate::year(time), mon = lubridate::month(time)) %>%
#    filter(yr %in% c(1971:2000, 2021:2050, 2071:2100)) %>%
#    mutate(yr = ifelse(yr %in% c(1971:2000), "past", ifelse(yr %in% c(2021:2050), "future", "extfuture"))) %>%
#    mutate(yr = factor(yr, levels=c("past", "future", "extfuture"), labels=c("1971-2000", "2021-2050", "2071-2100"))) %>%
#    group_by(x, y, mon, yr, gcm, rcp, rcm, ensemble) %>%
#    summarise(mn=mean(value), err=sd(value), mini=min(value), maxi=max(value))
#  rm(cordex_tasAdjust_bav); invisible(gc())
#  
#  # Plot map of temperature
#  dat <- tasAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn, na.rm=T))
#  col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
#  lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
#  dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(rcp~yr) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F------------------------------------------------------------------
#  # Plot month against year
#  tasAdjust_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(mon~yr) +
#    scale_fill_gradientn(name="tas", colours=whitered) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F------------------------------------------------------------------
#  # Plot season against year
#  dat <- tasAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
#    mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
#    mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SON"))) %>%
#    group_by(x, y, season, yr) %>% summarise(mn=mean(mn))
#  col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
#  lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
#  dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F, fig.width=8, fig.height=6---------------------------------------
#  # Plot summer/winter against year
#  dat1 <- tasAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>%
#    mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
#    group_by(x, y, season, yr) %>% summarise(mn=mean(mn)) %>% filter(season == "Summer")
#  col_val <- scales::rescale(quantile(dat1$mn, probs=seq(0,1,0.12)))
#  lim <- c(min(dat1$mn, na.rm=T), max(dat1$mn, na.rm=T))
#  p1 <- dat1 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")
#  
#  dat2 <- tasAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>%
#    mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
#    group_by(x, y, season, yr) %>% summarise(mn=mean(mn)) %>% filter(season == "Winter")
#  col_val <- scales::rescale(quantile(dat2$mn, probs=seq(0,1,0.12)))
#  lim <- c(min(dat2$mn, na.rm=T), max(dat2$mn, na.rm=T))
#  p2 <- dat2 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot map of temperature (1971-2000, 2021-205, 2071-2100) against GCM
#  tasAdjust_30yr %>% group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(gcm + rcm  ~ yr) +
#    scale_fill_gradientn(name="tas", colours=whitered) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=8----------------------------------------------
data("cordex_tasminAdjust_bav", package="bdc")

tasminAdjust_30yr <- cordex_tasminAdjust_bav %>% 
  mutate(yr = lubridate::year(time), mon = lubridate::month(time)) %>%
  filter(yr %in% c(1971:2000, 2021:2050, 2071:2100)) %>%
  mutate(yr = ifelse(yr %in% c(1971:2000), "past", ifelse(yr %in% c(2021:2050), "future", "extfuture"))) %>% 
  mutate(yr = factor(yr, levels=c("past", "future", "extfuture"), labels=c("1971-2000", "2021-2050", "2071-2100"))) %>%
  group_by(x, y, mon, yr, gcm, rcp, rcm, ensemble) %>% 
  summarise(mn=mean(value), err=sd(value), mini=min(value), maxi=max(value))
rm(cordex_tasminAdjust_bav); invisible(gc())

# Plot map of temperature
dat <- tasminAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn, na.rm=T))
col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(rcp~yr) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F------------------------------------------------------------------
#  # Plot month against year
#  tasminAdjust_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(mon~yr) +
#    scale_fill_gradientn(name="tasmin", colours=whitered) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot season against year
dat <- tasminAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>% 
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SON"))) %>%
  group_by(x, y, season, yr) %>% summarise(mn=mean(mn))
col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=8, fig.height=6-----------------------------------------------
# Plot summer/winter against year
dat1 <- tasminAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>% 
  mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
  group_by(x, y, season, yr) %>% summarise(mn=mean(mn)) %>% filter(season == "Summer")
col_val <- scales::rescale(quantile(dat1$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat1$mn, na.rm=T), max(dat1$mn, na.rm=T))
p1 <- dat1 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

dat2 <- tasminAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>% 
  mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
  group_by(x, y, season, yr) %>% summarise(mn=mean(mn)) %>% filter(season == "Winter")
col_val <- scales::rescale(quantile(dat2$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat2$mn, na.rm=T), max(dat2$mn, na.rm=T))
p2 <- dat2 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot map of temperature (1971-2000, 2021-205, 2071-2100) against GCM
#  tasminAdjust_30yr %>% group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(gcm + rcm + ensemble ~ yr) +
#    scale_fill_gradientn(name="tasmin", colours=whitered) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=8----------------------------------------------
data("cordex_tasmaxAdjust_bav", package="bdc")

tasmaxAdjust_30yr <- cordex_tasmaxAdjust_bav %>% 
  mutate(yr = lubridate::year(time), mon = lubridate::month(time)) %>%
  filter(yr %in% c(1971:2000, 2021:2050, 2071:2100)) %>%
  mutate(yr = ifelse(yr %in% c(1971:2000), "past", ifelse(yr %in% c(2021:2050), "future", "extfuture"))) %>% 
  mutate(yr = factor(yr, levels=c("past", "future", "extfuture"), labels=c("1971-2000", "2021-2050", "2071-2100"))) %>%
  group_by(x, y, mon, yr, gcm, rcp, rcm, ensemble) %>% 
  summarise(mn=mean(value), err=sd(value), mini=min(value), maxi=max(value))
rm(cordex_tasmaxAdjust_bav); invisible(gc())

# Plot map of temperature
dat <- tasmaxAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn, na.rm=T))
col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(rcp~yr) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F------------------------------------------------------------------
#  # Plot month against year
#  tasmaxAdjust_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(mon~yr) +
#    scale_fill_gradientn(name="tasmax", colours=whitered) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot season against year
dat <- tasmaxAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>% 
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SON"))) %>%
  group_by(x, y, season, yr) %>% summarise(mn=mean(mn))
col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) + 
  scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=8, fig.height=6-----------------------------------------------
# Plot summer/winter against year
dat1 <- tasmaxAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>% 
  mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
  group_by(x, y, season, yr) %>% summarise(mn=mean(mn)) %>% filter(season == "Summer")
col_val <- scales::rescale(quantile(dat1$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat1$mn, na.rm=T), max(dat1$mn, na.rm=T))
p1 <- dat1 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) + 
  scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

dat2 <- tasmaxAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>% 
  mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
  group_by(x, y, season, yr) %>% summarise(mn=mean(mn)) %>% filter(season == "Winter")
col_val <- scales::rescale(quantile(dat2$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat2$mn, na.rm=T), max(dat2$mn, na.rm=T))
p2 <- dat2 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr) + 
  scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot map of temperature (1971-2000, 2021-205, 2071-2100) against GCM
#  tasmaxAdjust_30yr %>% group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(gcm + rcm + ensemble  ~ yr) +
#    scale_fill_gradientn(name="tasmax", colours=whitered) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=8----------------------------------------------
data("cordex_prAdjust_bav", package="bdc")

prAdjust_30yr <- cordex_prAdjust_bav %>% 
  mutate(yr = lubridate::year(time), mon = lubridate::month(time)) %>%
  filter(yr %in% c(1971:2000, 2021:2050, 2071:2100)) %>%
  mutate(yr = ifelse(yr %in% c(1971:2000),"past", 
                     ifelse(yr %in% c(2021:2050), "future", "extfuture"))) %>% 
  mutate(yr = factor(yr, levels=c("past", "future", "extfuture"), 
                     labels=c("1971-2000", "2021-2050", "2071-2100"))) %>% 
  group_by(x, y, mon, yr, gcm, rcp, rcm, ensemble) %>% 
  summarise(mn=mean(value), err=sd(value), mini=min(value), maxi=max(value))
rm(cordex_prAdjust_bav); invisible(gc())

# Plot map of precipitation
dat <- prAdjust_30yr %>% group_by(x, y, yr, rcp, gcm, rcm, ensemble) %>% 
  summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(x,y, yr, rcp) %>%
  summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T))
col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(rcp~yr) + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F------------------------------------------------------------------
#  # Plot month against year
#  prAdjust_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(yr~mon) +
#    ggsci::scale_fill_gsea() + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot season against year
dat <- prAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SON"))) %>%
  group_by(x, y, season, yr, rcp, gcm, rcm, ensemble) %>% 
  summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(x,y, season, yr) %>%
  summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T))
col_val <- scales::rescale(quantile(dat$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat$mn, na.rm=T), max(dat$mn, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr, switch="y") + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + theme(strip.placement = "outside") + labs(x="", y="")

## ---- fig.width=8, fig.height=6-----------------------------------------------
# Plot summer/winter against year
dat1 <- prAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>% 
  mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
  group_by(x, y, season, yr, rcp, gcm, rcm, ensemble) %>% 
  summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(x,y, season, yr) %>%
  summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T)) %>% filter(season == "Summer")
col_val <- scales::rescale(quantile(dat1$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat1$mn, na.rm=T), max(dat1$mn, na.rm=T))
p1 <- dat1 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr, switch="y") + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + theme(strip.placement = "outside") + labs(x="", y="")

dat2 <- prAdjust_30yr %>% ungroup() %>% mutate(season=cut(mon, breaks=c(0,4,10,13), right=F)) %>% 
  mutate(season = factor(season, labels=c("Winter", "Summer", "Winter"))) %>%
   group_by(x, y, season, yr, rcp, gcm, rcm, ensemble) %>% 
  summarise(mn1=sum(mn, na.rm=T), err=sd(mn)) %>% group_by(x,y, season, yr) %>%
  summarise(mn=mean(mn1, na.rm=T), err=sd(mn1, na.rm=T)) %>% filter(season == "Winter")
col_val <- scales::rescale(quantile(dat2$mn, probs=seq(0,1,0.12)))
lim <- c(min(dat2$mn, na.rm=T), max(dat2$mn, na.rm=T))
p2 <- dat2 %>% ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(season~yr, switch="y") + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + theme(strip.placement = "outside") + 
  labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot map of precipitation against GCM
#  prAdjust_30yr %>% group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=mn)) + facet_grid(gcm + rcm + ensemble  ~ yr) +
#    scale_fill_gradientn(name="pr", colours=whiteblue) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F, fig.width=10, fig.height=7--------------------------------------
#  # Plot monthly change between 1971-2000 and 2021 - 2050
#  df <- tasAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tas_change <- df$`2021-2050`-df$`1971-2000`
#  df$tas_rel_change <- (df$`2021-2050`-df$`1971-2000`)/df$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  p1 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(.~rcp, nrow=1) +
#    scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
#  
#  col_val <- scales::rescale(quantile(df$tas_rel_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_rel_change, na.rm=T), max(df$tas_rel_change, na.rm=T))
#  p2 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_rel_change)) + facet_wrap(.~rcp, nrow=1) +
#    scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot monthly change between 1971-2000 and 2021 - 2050
#  df <- tasAdjust_30yr %>% group_by(x, y, mon, yr) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tas_change <- df$`2021-2050`-df$`1971-2000`
#  
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(.~mon) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F------------------------------------------------------------------
#  # Plot seasonal change between 1971-2000 and 2021 - 2050
#  df <- tasAdjust_30yr %>% ungroup() %>%
#    mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
#    mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
#    mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
#    group_by(x, y, yr, season) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr,mn)
#  df$tas_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(.~season) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F, fig.width=10, fig.height=9--------------------------------------
#  # Plot map of precipitation against GCM
#  df <- tasAdjust_30yr %>%
#    group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
#    tidyr::spread(yr, mn)
#  df$tas_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(gcm + rcm + ensemble  ~., ncol=5) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- eval=F, fig.width=10, fig.height=7--------------------------------------
#  # Plot change between 1971-2000 and 2021 - 2050
#  df <- tasAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tas_change <- df$`2071-2100`-df$`1971-2000`
#  df$tas_rel_change <- (df$`2071-2100`-df$`1971-2000`)/df$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  p1 <- df %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(.~rcp, nrow=1) +
#    scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
#  
#  col_val <- scales::rescale(quantile(df$tas_rel_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_rel_change, na.rm=T), max(df$tas_rel_change, na.rm=T))
#  p2 <- df %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=tas_rel_change)) + facet_wrap(.~rcp, nrow=1) +
#    scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot monthly change between 1971-2000 and 2071 - 2100
#  df <- tasAdjust_30yr %>% group_by(x, y, mon, yr) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tas_change <- (df$`2071-2100`-df$`1971-2000`)#/df$`1971-2000`*100
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(.~mon) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F------------------------------------------------------------------
#  # Plot seasonal change between 1971-2000 and 2071 - 2100
#  df <- tasAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
#    mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
#    mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
#    group_by(x, y, yr, season) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr,mn)
#  df$tas_change <- (df$`2071-2100`-df$`1971-2000`)#/df$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(.~season) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## ---- eval=F, fig.width=10, fig.height=9--------------------------------------
#  # Plot map of temperature against GCM
#  df <- tasAdjust_30yr %>%
#    group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
#    tidyr::spread(yr, mn)
#  df$tas_change <- (df$`2071-2100`-df$`1971-2000`)#/df$`1971-2000`*100
#  col_val <- scales::rescale(quantile(df$tas_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tas_change, na.rm=T), max(df$tas_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tas_change)) + facet_wrap(gcm + rcm + ensemble  ~ ., ncol=5) +
#    scale_fill_gradientn(name="tas", colours=whitered, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- fig.width=10, fig.height=7----------------------------------------------
# Plot change between 1971-2000 and 2021 - 2050
df <- tasminAdjust_30yr %>% select(-c(err, mini, maxi)) %>% tidyr::spread(yr, mn) %>%
  mutate(tasmin_change = `2021-2050`-`1971-2000`) %>%
  mutate(tasmin_rel_change = tasmin_change/`1971-2000`*100) %>% 
  group_by(x, y, rcp) %>% summarise(tasmin_change=mean(tasmin_change, na.rm=T),
                                    tasmin_rel_change=mean(tasmin_rel_change, na.rm=T))
summary(df)

col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
p1 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(unique(c(seq(min(df$tasmin_rel_change, na.rm=T), 0, length=5), 
                                  seq(0, max(df$tasmin_rel_change, na.rm=T), length=5))))
lim <- c(min(df$tasmin_rel_change, na.rm=T), max(df$tasmin_rel_change, na.rm=T))
p2 <- df %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_rel_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot monthly change between 1971-2000 and 2021 - 2050
#  df <- tasminAdjust_30yr %>% group_by(x, y, mon, yr) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tasmin_change <- df$`2021-2050`-df$`1971-2000`#/df$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(.~mon) +
#    scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot seasonal change between 1971-2000 and 2021 - 2050
df <- tasminAdjust_30yr %>% ungroup() %>% 
  mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
  group_by(x, y, yr, season) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr,mn)
df$tasmin_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(.~season) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=9----------------------------------------------
# Plot map of precipitation against GCM
df <- tasminAdjust_30yr %>% 
  group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
  tidyr::spread(yr, mn)
df$tasmin_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(gcm + rcm + ensemble  ~ ., ncol=5) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- fig.width=10, fig.height=7----------------------------------------------
# Plot change between 1971-2000 and 2021 - 2050
df <- tasminAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$tasmin_change <- df$`2071-2100`-df$`1971-2000`
df$tasmin_rel_change <- (df$`2071-2100`-df$`1971-2000`)/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
p1 <- df %>%
  ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(df$tasmin_rel_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmin_rel_change, na.rm=T), max(df$tasmin_rel_change, na.rm=T))
p2 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_rel_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot monthly change between 1971-2000 and 2071 - 2100
#  df <- tasminAdjust_30yr %>% group_by(x, y, mon, yr) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tasmin_change <- (df$`2071-2100`-df$`1971-2000`)#/df$`1971-2000`*100
#  col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(.~mon) +
#    scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot seasonal change between 1971-2000 and 2071 - 2100
df <- tasminAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
  group_by(x, y, yr, season) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$tasmin_change <- (df$`2071-2100`-df$`1971-2000`)#/df$`1971-2000`*100
col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(.~season) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=9----------------------------------------------
# Plot map of tasmin against GCM
df <- tasminAdjust_30yr %>% 
  group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
  tidyr::spread(yr, mn)
df$tasmin_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmin_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmin_change, na.rm=T), max(df$tasmin_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmin_change)) + facet_wrap(gcm + rcm + ensemble  ~ ., ncol=5) + 
  scale_fill_gradientn(name="tasmin", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- fig.width=10, fig.height=7----------------------------------------------
# Plot monthly change between 1971-2000 and 2021 - 2050
df <- tasmaxAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$tasmax_change <- df$`2021-2050`-df$`1971-2000`
df$tasmax_rel_change <- (df$`2021-2050`-df$`1971-2000`)/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
p1 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(df$tasmax_rel_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_rel_change, na.rm=T), max(df$tasmax_rel_change, na.rm=T))
p2 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_rel_change)) + facet_wrap(.~rcp, nrow=1) +
  scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot monthly change between 1971-2000 and 2021 - 2050
#  df <- tasmaxAdjust_30yr %>% group_by(x, y, mon, yr) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tasmax_change <- df$`2021-2050`-df$`1971-2000`#/df$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(.~mon) +
#    scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot seasonal change between 1971-2000 and 2021 - 2050
df <- tasmaxAdjust_30yr %>% ungroup() %>% 
  mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
  group_by(x, y, yr, season) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$tasmax_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(.~season) + 
  scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=9----------------------------------------------
# Plot map of tasmax against GCM
df <- tasmaxAdjust_30yr %>% 
  group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
  tidyr::spread(yr, mn)
df$tasmax_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(gcm + rcm + ensemble  ~ .,ncol=5) + 
  scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- fig.width=10, fig.height=7----------------------------------------------
# Plot change between 1971-2000 and 2021 - 2050
df <- tasmaxAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$tasmax_change <- df$`2071-2100`-df$`1971-2000`
df$tasmax_rel_change <- (df$`2071-2100`-df$`1971-2000`)/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
p1 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(df$tasmax_rel_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_rel_change, na.rm=T), max(df$tasmax_rel_change, na.rm=T))
p2 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_rel_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot monthly change between 1971-2000 and 2071 - 2100
#  df <- tasmaxAdjust_30yr %>% group_by(x, y, mon, yr) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$tasmax_change <- (df$`2071-2100`-df$`1971-2000`)#/df$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(.~mon) +
#    scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot seasonal change between 1971-2000 and 2071 - 2100
df <- tasmaxAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
  group_by(x, y, yr, season) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$tasmax_change <- (df$`2071-2100`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(.~season) + 
  scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=9----------------------------------------------
# Plot map of tasmax against GCM
df <- tasmaxAdjust_30yr %>% 
  group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
  tidyr::spread(yr, mn)
df$tasmax_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$tasmax_change, probs=seq(0,1,0.12)))
lim <- c(min(df$tasmax_change, na.rm=T), max(df$tasmax_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=tasmax_change)) + facet_wrap(gcm + rcm + ensemble  ~ ., ncol=5) + 
  scale_fill_gradientn(name="tasmax", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- fig.width=10, fig.height=7----------------------------------------------
# Plot change between 1971-2000 and 2021 - 2050
df <- prAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$pr_change <- (df$`2021-2050`-df$`1971-2000`)
df$pr_rel_change <- (df$`2021-2050`-df$`1971-2000`)/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$pr_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_change, na.rm=T), max(df$pr_change, na.rm=T))
p1 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="change", colours=whiteblue, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(df$pr_rel_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_rel_change, na.rm=T), max(df$pr_rel_change, na.rm=T))
p2 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_rel_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="% change", colours=whiteblue, limits=lim, values=col_val) + geom_sf(data=bavaria, fill="NA") + 
  coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- eval=F------------------------------------------------------------------
#  # Plot monthly change between 1971-2000 and 2021 - 2050
#  df <- prAdjust_30yr %>% group_by(x, y, mon, yr) %>% summarise(mn=mean(mn)) %>%
#    tidyr::spread(yr, mn)
#  df$pr_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(df$pr_change, probs=seq(0,1,0.12)))
#  lim <- c(min(df$pr_change, na.rm=T), max(df$pr_change, na.rm=T))
#  df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_change)) + facet_wrap(.~mon) +
#    scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## -----------------------------------------------------------------------------
# Plot seasonal change between 1971-2000 and 2021 - 2050
df <- prAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
  group_by(x, y, yr,season) %>% summarise(tot=sum(mn)) %>%
  tidyr::spread(yr, tot)
df$pr_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$pr_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_change, na.rm=T), max(df$pr_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_change)) + facet_wrap(.~season) + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=9----------------------------------------------
# Plot map of precipitation against GCM
df <- prAdjust_30yr %>% 
  group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
  tidyr::spread(yr, mn)
df$pr_change <- (df$`2021-2050`-df$`1971-2000`)#/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$pr_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_change, na.rm=T), max(df$pr_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_change)) + facet_wrap(gcm + rcm + ensemble  ~ ., ncol=5) + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- fig.width=10, fig.height=7----------------------------------------------
# Plot monthly change between 1971-2000 and 2071 - 2100
df <- prAdjust_30yr %>% group_by(x, y, yr, rcp) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$pr_change <- (df$`2071-2100`-df$`1971-2000`)
df$pr_rel_change <- (df$`2071-2100`-df$`1971-2000`)/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$pr_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_change, na.rm=T), max(df$pr_change, na.rm=T))
p1 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_change)) + facet_wrap(.~rcp, nrow=1) + 
  scale_fill_gradientn(name="change", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(df$pr_rel_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_rel_change, na.rm=T), max(df$pr_rel_change, na.rm=T))
p2 <- df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_rel_change)) + facet_wrap(.~rcp, nrow=1) +
  scale_fill_gradientn(name="% change", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## -----------------------------------------------------------------------------
# Plot seasonal change between 1971-2000 and 2071 - 2100
df <- prAdjust_30yr %>% ungroup() %>% mutate(mon=ifelse(mon==12,1,mon)) %>% # This make months unmeaningful
  mutate(season=cut(mon, breaks=c(1,3,5,8,12), right=F)) %>%
  mutate(season = factor(season, labels=c("DJF", "MAM", "JJA", "SSO"))) %>%
  group_by(x, y, yr, season) %>% summarise(mn=mean(mn)) %>%
  tidyr::spread(yr, mn)
df$pr_change <-(df$`2071-2100`-df$`1971-2000`)/df$`1971-2000`*100

col_val <- scales::rescale(quantile(df$pr_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_change, na.rm=T), max(df$pr_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_change)) + facet_wrap(.~season) + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="")

## ---- fig.width=10, fig.height=9----------------------------------------------
# Plot map of precipitation against GCM
df <- prAdjust_30yr %>% 
  group_by(x, y, yr, gcm, rcm, ensemble) %>% summarise(mn=mean(mn, na.rm=T)) %>%
  tidyr::spread(yr, mn)
df$pr_change <- (df$`2071-2100`-df$`1971-2000`)

col_val <- scales::rescale(quantile(df$pr_change, probs=seq(0,1,0.12)))
lim <- c(min(df$pr_change, na.rm=T), max(df$pr_change, na.rm=T))
df %>% ggplot() + geom_tile(aes(x=x, y=y, fill=pr_change)) + facet_wrap(gcm + rcm + ensemble  ~ ., ncol=5) + 
  scale_fill_gradientn(name="pr", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() + labs(x="", y="") + theme(legend.position=c(0.9,0.1))

## ---- eval=F, fig.width=10, fig.height=7--------------------------------------
#  #' For every grid cell calculate monthly mean per year using as.yearmon() from the zoo package.
#  #' Make a linear model using do() and extract Intercept, Slope and R2 values per grid cell.
#  data("cordex_tasAdjust_bav", package="bdc")
#  tasAdjust_lm <- cordex_tasAdjust_bav %>% mutate(yrmon = zoo::as.yearmon(time)) %>%
#    mutate(x = round(x, digits=5), y = round(y, digits=5)) %>%
#    group_by(x, y, rcp) %>% do(mod = lm(value ~ yrmon, data = .)) %>%
#    mutate(Intercept = summary(mod)$coeff[1],
#           Slope = summary(mod)$coeff[2],
#           R2 = summary(mod)$r.squared) %>%
#    dplyr::select(-mod); rm(cordex_tasAdjust_bav); invisible(gc())
#  
#  col_val <- scales::rescale(quantile(tasAdjust_lm$Slope, probs=seq(0,1,0.12)))
#  lim <- c(min(tasAdjust_lm$Slope, na.rm=T), max(tasAdjust_lm$Slope, na.rm=T))
#  p1 <- tasAdjust_lm %>% ggplot() + geom_tile(aes(x=x, y=y, fill=Slope)) +
#    scale_fill_gradientn(name="Slope", colours=whitered, limits=lim, values=col_val) +
#    facet_wrap(.~rcp, nrow=1) + geom_sf(data=bavaria, fill="NA") +
#    coord_sf() + theme_few() + labs(x="", y="")
#  
#  col_val <- scales::rescale(quantile(tasAdjust_lm$R2, probs=seq(0,1,0.12)))
#  lim <- c(min(tasAdjust_lm$R2, na.rm=T), max(tasAdjust_lm$R2, na.rm=T))
#  p2 <- tasAdjust_lm %>% ggplot() + geom_tile(aes(x=x, y=y, fill=R2)) +
#    scale_fill_gradientn(name="R2", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + facet_wrap(.~rcp, nrow=1) +
#    coord_sf() + theme_few() + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

## ----fig.width=10, fig.height=7-----------------------------------------------
#' For every grid cell calculate monthly mean per year using as.yearmon() from the zoo package.
#' Make a linear model using do() and extract Intercept, Slope and R2 values per grid cell.
data("cordex_tasminAdjust_bav", package="bdc")
tasminAdjust_lm <- cordex_tasminAdjust_bav %>% mutate(yrmon = zoo::as.yearmon(time)) %>%
  mutate(x = round(x, digits=5), y = round(y, digits=5)) %>% 
  group_by(x, y, rcp) %>% do(mod = lm(value ~ yrmon, data = .)) %>%
  mutate(Intercept = summary(mod)$coeff[1],
         Slope = summary(mod)$coeff[2],
         R2 = summary(mod)$r.squared) %>%
  dplyr::select(-mod); rm(cordex_tasminAdjust_bav); invisible(gc())

col_val <- scales::rescale(quantile(tasminAdjust_lm$Slope, probs=seq(0,1,0.12)))
lim <- c(min(tasminAdjust_lm$Slope, na.rm=T), max(tasminAdjust_lm$Slope, na.rm=T))
p1 <- tasminAdjust_lm %>% ggplot() + geom_tile(aes(x=x, y=y, fill=Slope)) + 
  scale_fill_gradientn(name="Slope", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + facet_wrap(.~rcp, nrow=1) + coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(tasminAdjust_lm$R2, probs=seq(0,1,0.12)))
lim <- c(min(tasminAdjust_lm$R2, na.rm=T), max(tasminAdjust_lm$R2, na.rm=T))
p2 <- tasminAdjust_lm %>% ggplot() + geom_tile(aes(x=x, y=y, fill=R2)) + 
  scale_fill_gradientn(name="R2", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + facet_wrap(.~rcp, nrow=1) + coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ----fig.width=10, fig.height=7-----------------------------------------------
#' For every grid cell calculate monthly mean per year using as.yearmon() from the zoo package.
#' Make a linear model using do() and extract Intercept, Slope and R2 values per grid cell.
data("cordex_tasmaxAdjust_bav", package="bdc")
tasmaxAdjust_lm <- cordex_tasmaxAdjust_bav %>% mutate(yrmon = zoo::as.yearmon(time)) %>%
  mutate(x = round(x, digits=5), y = round(y, digits=5)) %>% 
  group_by(x, y, rcp) %>% do(mod = lm(value ~ yrmon, data = .)) %>%
  mutate(Intercept = summary(mod)$coeff[1],
         Slope = summary(mod)$coeff[2],
         R2 = summary(mod)$r.squared) %>%
  dplyr::select(-mod); rm(cordex_tasmaxAdjust_bav); invisible(gc())

col_val <- scales::rescale(quantile(tasmaxAdjust_lm$Slope, probs=seq(0,1,0.12)))
lim <- c(min(tasmaxAdjust_lm$Slope, na.rm=T), max(tasmaxAdjust_lm$Slope, na.rm=T))
p1 <- tasmaxAdjust_lm %>% ggplot() + geom_tile(aes(x=x, y=y, fill=Slope)) + 
  scale_fill_gradientn(name="Slope", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + facet_wrap(.~rcp, nrow=1) + coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(tasmaxAdjust_lm$R2, probs=seq(0,1,0.12)))
lim <- c(min(tasmaxAdjust_lm$R2, na.rm=T), max(tasmaxAdjust_lm$R2, na.rm=T))
p2 <- tasmaxAdjust_lm %>% ggplot() + geom_tile(aes(x=x, y=y, fill=R2)) + 
  scale_fill_gradientn(name="R2", colours=whitered, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + facet_wrap(.~rcp, nrow=1) + coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## ---- fig.width=10, fig.height=7----------------------------------------------
#' For every grid cell calculate monthly mean per year using as.yearmon() from the zoo package.
#' Make a linear model using do() and extract Intercept, Slope and R2 values per grid cell.
data("cordex_prAdjust_bav", package="bdc")
prAdjust_lm <- cordex_prAdjust_bav %>% mutate(yrmon = zoo::as.yearmon(time)) %>%
  mutate(x = round(x, digits=5), y = round(y, digits=5)) %>% 
  group_by(x, y, rcp) %>% do(mod = lm(value ~ yrmon, data = .)) %>% 
  mutate(Intercept = summary(mod)$coeff[1],
         Slope = summary(mod)$coeff[2],
         R2 = summary(mod)$r.squared) %>%
  dplyr::select(-mod); rm(cordex_prAdjust_bav); invisible(gc())

col_val <- scales::rescale(quantile(prAdjust_lm$Slope, probs=seq(0,1,0.12)))
lim <- c(min(prAdjust_lm$Slope, na.rm=T), max(prAdjust_lm$Slope, na.rm=T))
p1 <- prAdjust_lm %>% ungroup() %>% ggplot() + geom_tile(aes(x=x, y=y, fill=Slope)) + 
  scale_fill_gradientn(name="Slope", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + facet_wrap(.~rcp, nrow=1) + 
  coord_sf() + theme_few() + labs(x="", y="")

col_val <- scales::rescale(quantile(prAdjust_lm$R2, probs=seq(0,1,0.12)))
lim <- c(min(prAdjust_lm$R2, na.rm=T), max(prAdjust_lm$R2, na.rm=T))
p2 <- prAdjust_lm %>% ggplot() + geom_tile(aes(x=x, y=y, fill=R2)) + 
  scale_fill_gradientn(name="R2", colours=whiteblue, limits=lim, values=col_val) + 
  geom_sf(data=bavaria, fill="NA") + facet_wrap(.~rcp, nrow=1) + 
  coord_sf() + theme_few() + labs(x="", y="")
p1 + p2 + plot_layout(ncol=1)

## -----------------------------------------------------------------------------
data("cordex_bioclim_bav", package="bdc") # 66123

## ---- fig.width=10, fig.height=10---------------------------------------------
tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  mutate(var = factor(var, levels=paste0("bio", 1:19))) %>% ggplot() + geom_violin(aes(x=yr, y=value, fill=rcp)) + 
  facet_wrap(.~var, scales="free_y", strip.position = "left", ncol=4) + 
  theme_few() + theme(strip.background = element_blank(),
                       legend.position=c(0.9,0.05), strip.placement = "outside",
                       axis.text.x = element_text(angle=45, vjust=0.5)) + 
  scale_fill_manual(name="", values=c("#0099B4FF", "#ADB6B6FF", "#AD002AFF")) + labs(x="", y="")

## ---- fig.width=10, fig.height=10---------------------------------------------
tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  filter(var == "bio1") %>% ggplot() + geom_violin(aes(x=yr, y=value, fill=rcp)) + 
  facet_wrap(.~gcm+rcm+ensemble, ncol=5) + 
  theme_few() + theme(legend.position=c(0.9,0.2), axis.text.x = element_text(angle=45, vjust=0.5)) + 
  scale_fill_manual(name="", values=c("#0099B4FF", "#ADB6B6FF", "#AD002AFF")) + labs(x="", y="")

## ---- fig.width=10, fig.height=10---------------------------------------------
tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  filter(var == "bio12") %>% ggplot() + geom_violin(aes(x=yr, y=value, fill=rcp)) + 
  facet_wrap(.~gcm+rcm+ensemble, ncol=5) + 
  theme_few() + theme(legend.position=c(0.9,0.2), axis.text.x = element_text(angle=45, vjust=0.5)) + 
  scale_fill_manual(name="", values=c("#0099B4FF", "#ADB6B6FF", "#AD002AFF")) + labs(x="", y="")

## -----------------------------------------------------------------------------
dat <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  mutate(var = factor(var, levels=paste0("bio", 1:19))) %>% 
  filter(var == "bio1") %>%
  group_by(x, y, yr, rcp, var) %>% summarise(value=mean(value))
col_val <- scales::rescale(quantile(dat$value, probs=seq(0,1,0.12)))
lim <- c(min(dat$value, na.rm=T), max(dat$value, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + 
  facet_grid(rcp~yr, switch = "y") + geom_sf(data=bavaria, fill="NA") + 
  scale_fill_gradientn(name="bio1", colours=whitered, limits=lim, values=col_val) + 
  coord_sf() + theme_few() + theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + 
  labs(x="", y="")

## -----------------------------------------------------------------------------
dat <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  mutate(var = factor(var, levels=paste0("bio", 1:19))) %>% 
  filter(var == "bio4") %>%
  group_by(x, y, yr, rcp, var) %>% summarise(value=mean(value))
col_val <- scales::rescale(quantile(dat$value, probs=seq(0,1,0.12)))
lim <- c(min(dat$value, na.rm=T), max(dat$value, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + 
  facet_grid(rcp~yr, switch = "y") + geom_sf(data=bavaria, fill="NA") + 
  scale_fill_gradientn(name="bio4", colours=whitered, limits=lim, values=col_val) + 
  coord_sf() + theme_few() + theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + 
  labs(x="", y="")

## -----------------------------------------------------------------------------
dat <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  mutate(var = factor(var, levels=paste0("bio", 1:19))) %>% 
  filter(var == "bio5") %>%
  group_by(x, y, yr, rcp, var) %>% summarise(value=mean(value))
col_val <- scales::rescale(quantile(dat$value, probs=seq(0,1,0.12)))
lim <- c(min(dat$value, na.rm=T), max(dat$value, na.rm=T))

dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + geom_sf(data=bavaria, fill="NA") + 
  facet_grid(rcp~yr, switch = "y") + scale_fill_gradientn(name="bio5", colours=whitered, limits=lim, values=col_val) + 
  coord_sf() + theme_few() + theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + 
  labs(x="", y="")

## -----------------------------------------------------------------------------
dat <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  mutate(var = factor(var, levels=paste0("bio", 1:19))) %>% 
  filter(var == "bio12") %>%
  group_by(x, y, yr, rcp, var) %>% summarise(value=mean(value))
col_val <- scales::rescale(quantile(dat$value, probs=seq(0,1,0.12)))
lim <- c(min(dat$value, na.rm=T), max(dat$value, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + geom_sf(data=bavaria, fill="NA") + 
  facet_grid(rcp~yr, switch = "y") + scale_fill_gradientn(name="bio12", colours=whiteblue, limits=lim, values=col_val) + 
  coord_sf() + theme_few() + theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + 
  labs(x="", y="")

## -----------------------------------------------------------------------------
dat <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
  mutate(var = factor(var, levels=paste0("bio", 1:19))) %>% 
  filter(var == "bio15") %>%
  group_by(x, y, yr, rcp, var) %>% summarise(value=mean(value))
col_val <- scales::rescale(quantile(dat$value, probs=seq(0,1,0.12)))
lim <- c(min(dat$value, na.rm=T), max(dat$value, na.rm=T))
dat %>% ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + geom_sf(data=bavaria, fill="NA") + 
  facet_grid(rcp~yr, switch = "y") + scale_fill_gradientn(name="bio15", colours=whiteblue, limits=lim, values=col_val) + 
  coord_sf() + theme_few() + theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + 
  labs(x="", y="")

## ---- eval=F, fig.width=10, fig.height=7--------------------------------------
#  # Plot bioclimatic changes between 1971-2000 and 2021 - 2050
#  bioclim_30yr <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
#    group_by(x, y, yr, var, rcp) %>% summarise(value=mean(value)) %>%
#    tidyr::spread(yr,value) %>% filter(var == "bio1")
#  bioclim_30yr$change <- (bioclim_30yr$`2021-2050`-bioclim_30yr$`1971-2000`)
#  bioclim_30yr$rel_change <- (bioclim_30yr$`2021-2050`-bioclim_30yr$`1971-2000`)/bioclim_30yr$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$change, na.rm=T), max(bioclim_30yr$change, na.rm=T))
#  p1 <- bioclim_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$rel_change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$rel_change, na.rm=T), max(bioclim_30yr$rel_change, na.rm=T))
#  p2 <- bioclim_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=rel_change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

## ---- eval=F, fig.width=10, fig.height=7--------------------------------------
#  # Plot bioclimatic changes between 1971-2000 and 2021 - 2050
#  bioclim_30yr <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
#    group_by(x, y, yr, var, rcp) %>% summarise(value=mean(value)) %>%
#    tidyr::spread(yr,value) %>% filter(var == "bio12")
#  bioclim_30yr$change <- (bioclim_30yr$`2021-2050`-bioclim_30yr$`1971-2000`)
#  bioclim_30yr$rel_change <- (bioclim_30yr$`2021-2050`-bioclim_30yr$`1971-2000`)/bioclim_30yr$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$change, na.rm=T), max(bioclim_30yr$change, na.rm=T))
#  p1 <- bioclim_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="change", colours=whiteblue, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$rel_change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$rel_change, na.rm=T), max(bioclim_30yr$rel_change, na.rm=T))
#  p2 <- bioclim_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=rel_change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="% change", colours=whiteblue, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

## ---- eval=F, fig.width=10, fig.height=7--------------------------------------
#  # Plot bioclimatic changes between 1971-2000 and 2021 - 2050
#  bioclim_30yr <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
#    group_by(x, y, yr, var, rcp) %>% summarise(value=mean(value)) %>%
#    tidyr::spread(yr,value) %>% filter(var == "bio1")
#  bioclim_30yr$change <- (bioclim_30yr$`2071-2100`-bioclim_30yr$`1971-2000`)
#  bioclim_30yr$rel_change <- (bioclim_30yr$`2071-2100`-bioclim_30yr$`1971-2000`)/bioclim_30yr$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$change, na.rm=T), max(bioclim_30yr$change, na.rm=T))
#  p1 <- bioclim_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$rel_change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$rel_change, na.rm=T), max(bioclim_30yr$rel_change, na.rm=T))
#  p2 <- bioclim_30yr %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=rel_change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="% change", colours=whitered, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

## ---- eval=F, fig.width=10, fig.height=7--------------------------------------
#  # Plot bioclimatic changes between 1971-2000 and 2021 - 2050
#  bioclim_30yr <- tidyr::gather(cordex_bioclim_bav, var, value, -c(x,y,yr,gcm,rcp,rcm,ensemble,rs)) %>%
#    group_by(x, y, yr, var, rcp) %>% summarise(value=mean(value)) %>%
#    tidyr::spread(yr,value) %>% filter(var == "bio12")
#  bioclim_30yr$change <- (bioclim_30yr$`2071-2100`-bioclim_30yr$`1971-2000`)
#  bioclim_30yr$rel_change <- (bioclim_30yr$`2071-2100`-bioclim_30yr$`1971-2000`)/bioclim_30yr$`1971-2000`*100
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$change, na.rm=T), max(bioclim_30yr$change, na.rm=T))
#  p1 <- bioclim_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="change", colours=whiteblue, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  
#  col_val <- scales::rescale(quantile(bioclim_30yr$rel_change, probs=seq(0,1,0.12)))
#  lim <- c(min(bioclim_30yr$rel_change, na.rm=T), max(bioclim_30yr$rel_change, na.rm=T))
#  p2 <- bioclim_30yr %>% ggplot() + geom_tile(aes(x=x, y=y, fill=rel_change)) + facet_grid(var~rcp, switch="y") +
#    scale_fill_gradientn(name="% change", colours=whiteblue, limits=lim, values=col_val) +
#    geom_sf(data=bavaria, fill="NA") + coord_sf() + theme_few() +
#    theme(strip.placement = "outside", axis.text.x = element_text(angle=45, vjust=0.5)) + labs(x="", y="")
#  p1 + p2 + plot_layout(ncol=1)

