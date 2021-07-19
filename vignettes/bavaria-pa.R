## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = T, comment = NA, warning=F, message=F, eval=T, echo=T, error=F, 
  comment = "#>", dpi=100
)

## ----pa-overview, fig.width=6, fig.height=7-----------------------------------
# Load bdc & ggplot2 package
library(bdc); library(dplyr); library(sf); library(ggplot2); library(tidyr)

# Create colour scheme
redblue <- rev(colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                                  "yellow", "#FF7F00", "red", "#7F0000"))(255))

# Load pa data from BDC package
data(pa_bav_tk4tel, package="bdc")
unique(pa_bav_tk4tel$iucn_cat)
pa_bav_tk4tel$iucn_cat <- factor(pa_bav_tk4tel$iucn_cat, 
                                 levels=c("I", "II", "III", "IV", "V", "Not.Assigned", 
                                          "Not.Applicable", "Not.Reported", "Total"), 
                                 labels=c("I", "II", "III", "IV", "V", "Not Assigned", 
                                          "Not Applicable", "Not Reported", "Total"))

# Plot percentage cover by iucn cat
pa_bav_tk4tel %>% filter(var == "perc_cov") %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + 
  facet_wrap(iucn_cat ~.) + coord_sf(ndiscr=0) + 
  theme_classic() + labs(x="", y="") + 
  scale_fill_gradientn(name="% Cover", na.value= "grey50", colours=redblue) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position=c(0.85,0.15))

# Plot number of PAs by iucn cat
pa_bav_tk4tel %>% filter(var == "no_pa") %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + 
  facet_wrap(iucn_cat ~.) + coord_sf(ndiscr=0) + 
  theme_classic() + labs(x="", y="") + 
  scale_fill_gradientn(name="Number of PAs", na.value= "grey50", colours=redblue) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position=c(0.85,0.15))

# Plot minimum year by iucn cat
pa_bav_tk4tel %>% filter(var == "min_year") %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=value)) + 
  facet_wrap(iucn_cat ~.) + coord_sf(ndiscr=0) + 
  theme_classic() + labs(x="", y="") + 
  scale_fill_gradientn(name="First year", na.value= "grey50", colours= redblue) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position=c(0.85,0.15))

## ----pa-cormat, fig.width=6, fig.height=6-------------------------------------
# GGally, to assess the distribution and correlation of variables 
library(GGally)
data("dist_pa_bav_tk4tel")
dist_pa_bav_tk4tel$value <- dist_pa_bav_tk4tel$dist
dist_pa_bav_tk4tel$var <- "dist"

dat <- pa_bav_tk4tel %>% bind_rows(dist_pa_bav_tk4tel %>% dplyr::select(-dist)) %>% 
  tidyr::drop_na() %>% tidyr::spread(var, value) %>% 
  mutate(iucn_cat2 = factor(.$iucn_cat, 
                            levels=c("Ia", "Ib", "II", "III", "IV", "V", "VI", 
                                     "Not.Assigned", "Not.Applicable", 
                                     "Not.Reported", "Total"), 
                            labels=c("I", "I", "II", "III", "IV", "V", "VI", 
                                     "Not designated", "Not designated", 
                                     "Not designated", "Total"))) %>% 
  dplyr::select(-iucn_cat) %>% group_by(x,y, iucn_cat2) %>%
  summarise(min_year = min(min_year, na.rm=T),
            no_pa = sum(no_pa, na.rm=T),
            perc_cov = sum(perc_cov, na.rm=T),
            dist = min(dist, na.rm=T)) %>% ungroup() %>%
  tidyr::gather(var, value, -c(x,y,iucn_cat2)) %>% 
  unite("var", c(iucn_cat2, var), sep="_") %>% 
  tidyr::spread(var, value) %>% 
  dplyr::select(-c(x,y)) %>% tidyr::drop_na()
dat <- dat %>% dplyr::select(c(Total_dist, Total_min_year, Total_no_pa, Total_perc_cov, 
                               II_no_pa, II_perc_cov, III_no_pa, III_perc_cov, 
                               IV_no_pa, IV_perc_cov))
#summary(dat)

# Check correlations (as scatterplots), distribution and print correlation coefficient 
#ggpairs(dat) 

# Check correlation between variables
#cor(dat) 

# Nice visualization of correlations
ggcorr(dat, method = c("everything", "pearson"), label=T) 

## ----pa-summary_table, asis="TRUE"--------------------------------------------
# Load bavarian PA data
data(pa_bav_tk4tel)
pa_bav_tk4tel$iucn_cat <- factor(pa_bav_tk4tel$iucn_cat, 
                                 levels=c("I", "II", "III", "IV", "V", "Not.Assigned", 
                                          "Not.Applicable", "Not.Reported", "Total"), 
                                 labels=c("I", "II", "III", "IV", "V", "Not Assigned", 
                                          "Not Applicable", "Not Reported", "Total"))

pa_bav_tk4tel <- pa_bav_tk4tel %>% filter(var == "perc_cov") %>% select(-var)
colnames(pa_bav_tk4tel) <- c("x", "y", "IUCN Category", "perc_cov")
pa_bav_tk4tel %>% group_by(`IUCN Category`) %>% 
  mutate(perc_cov = tidyr::replace_na(perc_cov, 0)) %>%
  summarise(`Minimum`=min(perc_cov), `Mean`=mean(perc_cov), `Max`=max(perc_cov)) %>% 
  knitr::kable(format="pipe")

## ----pa-dist, fig.width=6, fig.height=6---------------------------------------
library(scico); library(ggthemes)
bavaria_gk <- sf::st_transform(bavaria, crs=31468)
data("dist_pa_bav_tk4tel")
dist_pa_bav_tk4tel %>% filter(iucn_cat == "Total") %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=dist)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_scico(name="PA distance", palette="roma") + theme_map() + 
  theme(legend.position = c(0.1,0.23))

## ----pa-map_eng, fig.width=8, fig.height=6------------------------------------
data("pa_bav")
ggplot() + geom_sf(data=pa_bav, aes(fill=DESIG_ENG), col=NA) +
  geom_sf(data=bavaria_gk, fill=NA) + theme_minimal() 

## ----pa-map_eng_sub, fig.width=7, fig.height=8--------------------------------
pa_bav$DESIG_ENG <- factor(pa_bav$DESIG_ENG)
levels(pa_bav$DESIG_ENG)[5] <- "Ramsar Site"
levels(pa_bav$DESIG_ENG)[6] <- "Site of Community Importance"
levels(pa_bav$DESIG_ENG)[7] <- "Special Protection Area"

# names of the areas to plot and their colour 
my_cols = data.frame(Names=c("Special Protection Area", #(Birds Directive)"
                             "Site of Community Importance", #(Habitats Directive)
                             "Nature Reserve",
                             "National Park",
                             "Ramsar Site"), #, Wetland of International Importance
                     Color=c("hotpink3",
                             "burlywood",
                             "darkolivegreen1",
                             "forestgreen",
                             "steelblue"))
my_cols$Names = as.character(my_cols$Names)  
my_cols$Color = as.character(my_cols$Color) # making sure they are not factors

#plot bavaria  
par(bg="transparent")
plot(sf::st_geometry(bavaria_gk))
#loop through areas and plot them
for(i in 1:length(my_cols[,1])){
  pa_bav %>% filter(DESIG_ENG == my_cols[i,1]) %>% 
    st_geometry() %>% plot(col=my_cols[i,2], add=T)
}
#plot bavaria again
plot(sf::st_geometry(bavaria_gk), add=T) 
# the areas some-times overwrite the borders, that's not very pretty
legend("bottom", legend = c(my_cols[,1]), cex =0.8, fill=my_cols[,2], 
       bty="n", xpd = T, inset = c(0,-0.1))

## ----pa-map_deu_sub, fig.width=8, fig.height=8--------------------------------
#For German names change legend to names_D
names_D = c("Vogelschutzgebiet", "FFH-Gebiet", "Naturschutzgebiet", "Nationalpark",
            "Ramsar Schutzgebiet")

par(bg="transparent")
#plot bavaria  
plot(sf::st_geometry(bavaria_gk))
#loop through areas and plot them
for(i in 1:length(my_cols[,1])){
  pa_bav %>% filter(DESIG_ENG == my_cols[i,1]) %>% 
    st_geometry() %>% plot(col=my_cols[i,2], border=F, add=T)
}
#plot bavaria again
plot(sf::st_geometry(bavaria_gk), add=T) 
# the areas some-times overwrite the borders, that's not very pretty
legend("left", legend = names_D, cex = 1.2, fill=my_cols[,2], 
       bty="n", xpd = T, inset = c(-0.1,-0.1))
rm(list=ls()); invisible(gc())

