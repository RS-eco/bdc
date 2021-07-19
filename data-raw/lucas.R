#' ## LUCAS (Land use and land cover survey) data
#'
#' https://ec.europa.eu/eurostat/statistics-explained/index.php/LUCAS_-_Land_use_and_land_cover_survey

# Set file directory
filedir <- "/media/matt/Data/Documents/Wissenschaft/Data/LUCAS/"
if(!dir.exists(filedir)){dir.create(filedir)}

#' LUCAS-Database
#' https://ec.europa.eu/eurostat/web/lucas/data/database

#' ## Land cover and land use, landscape (LUCAS) (lan) 	

#' Land cover overview by NUTS 2 regions (lan_lcv_ovw) 	 
#' Land covered by artificial surfaces by NUTS 2 regions (lan_lcv_art) 	 
#' Land covered by artificial surfaces - index (lan_lcv_arti) 	 
#' Land use overview by NUTS 2 regions (lan_use_ovw) 	 
#' Land cover for FAO Forest categories by NUTS 2 regions (lan_lcv_fao) 	 
#' Settlement area (lan_settl) 	 

#download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/lan_lcv_ovw.tsv.gz",
#              destfile=paste0(filedir, "/lan_lcv_ovw.tsv.gz"))
#download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/lan_lcv_art.tsv.gz",
#              destfile=paste0(filedir, "/lan_lcv_art.tsv.gz"))
#download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/lan_lcv_arti.tsv.gz",
#              destfile=paste0(filedir, "/lan_lcv_arti.tsv.gz"))
#download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/lan_use_ovw.tsv.gz",
#              destfile=paste0(filedir, "/lan_use_ovw.tsv.gz"))
#download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/lan_lcv_fao.tsv.gz",
#              destfile=paste0(filedir, "/lan_lcv_fao.tsv.gz"))
#download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/lan_settl.tsv.gz",
#              destfile=paste0(filedir, "/lan_settl.tsv.gz"))

files <- list.files(filedir, pattern=".tsv.gz", full.names=T)
dat <- lapply(files, readr::read_tsv)

#' LUCAS-Grid
#' https://ec.europa.eu/eurostat/web/lucas/data/lucas-grid

#download.file("https://ec.europa.eu/eurostat/documents/205002/7329820/CSV-GRID-20171113.7z", 
#              destfile=paste0(filedir, "CSV-GRID-20171113.7z"))

dat <- readr::read_csv(paste0(filedir, "GRID_CSVEXP_20171113.csv"))
dat <- dplyr::filter(dat, NUTS0_13 == "DE")
unique(dat$NUTS2_13)

#' LUCAS-Primary data available for 2006, 2009, 2012, 2015, 2018
#' https://ec.europa.eu/eurostat/web/lucas/data/primary-data/

#' LUCAS micro data for Germany
#download.file("https://ec.europa.eu/eurostat/documents/205002/8072634/DE-2018-20190611.csv",
#              destfile=paste0(filedir, "DE-2018-20190611.csv")) # 2018
#download.file("https://ec.europa.eu/eurostat/documents/205002/8072634/DE_2015_20180724.csv",
#              destfile=paste0(filedir, "DE_2015_20180724.csv") # 2015
#download.file("https://ec.europa.eu/eurostat/documents/205002/8072634/DE_2012_20160926.csv",
#              destfile=paste0(filedir, "DE_2012_20160926.csv") # 2012
#download.file("https://ec.europa.eu/eurostat/documents/205002/8072634/DE_2009_20161125.csv",
#              destfile=paste0(filedir, "DE_2009_20161125.csv") # 2009

files <- c(list.files(filedir, pattern="DE_20", full.names=T), list.files(filedir, pattern="DE-20", full.names=T))
dat <- lapply(files, readr::read_csv)

#' LUCAS transect data for 2012 & 2015
#download.file("https://ec.europa.eu/eurostat/documents/205002/6786255/DE_TR_2012_20160929.csv", destfile=paste0(filedir, "DE_TR_2012_20160929.csv"))
#download.file("https://ec.europa.eu/eurostat/documents/205002/6786255/DE_TR_2015_20160927.csv", destfile=paste0(filedir, "DE_TR_2015_20160927.csv"))

dat_2012 <- readr::read_csv(paste0(filedir, "DE_TR_2012_20160929.csv"))
dat_2015 <- readr::read_csv(paste0(filedir, "DE_TR_2015_20160927.csv"))

#' LUCAS transect indicators 2009 & 2012
#' https://ec.europa.eu/eurostat/documents/205002/6163643/DE-tr2009.xls
#' https://ec.europa.eu/eurostat/documents/205002/6163643/DE-tr2012.xls

##############################

#' LUCAS 2009 top-soil data
#' requires Data Access request and permission
#' more details can be found below



##########

#' SPECIFICATION OF LUCAS_SOIL DATA

#' Database: The LUCAS_SOIL data are :

#' In MS Excel format for the soil properties data: LUCAS_TOPSOIL_v1.xlsx
#' 1 formats for Soil Multispectral data: LUCAS_TOPSOIL_v1_spectral.rdata (RDATA)

#' Reports (Proposed Citation): The LUCAS_SOIL data are documented in the report:

#' Orgiazzi, A., Ballabio, C., Panagos, P., Jones, A., Fernández-Ugalde, O. LUCAS Soil, the largest expandable soil dataset for Europe: A review. 2017. European Journal of Soil Science, Article in Press, DOI: 10.1111/ejss.12499
#' "Toth, G., Jones, A., Montanarella, L. (eds.) 2013. LUCAS Topsoil Survey. Methodology, data and results. JRC Technical Reports. Luxembourg. Publications Office of the European Union, EUR 26102 - Scientific and Technical Research series - ISSN 1831-9424 (online); ISBN 978-92-79-32542-7; doi: 10.2788/97922"

##########

#' Through this page you will be able to download the LUCAS_TOPSOIL data (soil properties data; multispectral absorbance data).
#' There are three download files.

#' LUCAS_TOPSOIL_v1.zip: : an Excel file that contains the geographical coordinates of the location where the data were sampled and for each location the values for a number of properties. The data in this file are documented in the report: LUCAS Topsoil Survey. Methodology, data and results.
#' LUCAS_Romania_Bulgaria_2012.zip: 2 Excel files containing data for Romania and Bulgaria, from the LUCAS 2012 campaign; (27/09/2019, important note: the values for nitrogen N are in mg/100g)
#' Multispectral reflectance data are available in 2 format: zipped RDATA (0.5 GB) and CSV (0.7 GB)

#' In the RDATA file, each row contains spectral data for a sample; for each sample two replicates were made (sample was scanned twice; in two different directions) :
#' - the first column contains the ID of the sample, extended with '-1' or '-2', indicating the first and second replicate;
#' - the other columns are the absorbance values at different wavelengths; there are 4200 values in a row corresponding to the wavelengths at a resolution of 0.5 nm.

#' The RDATA file has also been exported to .csv format.

#' (05/12/2018) For the convenience of the user, and based on the data that are in the files LUCAS_TOPSOIL_v1 and LUCAS_Romania_Bulgaria_2012, a package has been created that contains a number of shapefiles:

#' Shapefile SoilAttr_LUCAS2009: all 19969 LUCAS Topsoil 2009 points minus the 109 points for Malta and Cyprus, thus in total 19,860 points
#' Shapefile  SoilAttr_BG_RO: 2034 points for Romania and Bulgaria (part of 2012 LUCAS campaign); (27/09/2019, important note: the values for nitrogen N are in mg/100g)
#' Shapefile  SoilAttr_CYP_MLT: 109 points for Cyprus and Malta
#' Shapefile  SoilAttr_ICELAND: 65 points for Iceland

#' Download the package with shapefiles package-for-ESDAC-20181205.zip; please consult the readme file (inluded in the package) before using the data.

#'(27/09/2019) important notes in relation to the package-for-ESDAC-20181205.zip package :

#' the values for nitrogen N in the shapefile SoilAttr_LUCAS2009 have been erroneously rounded to integers, and therefore should not be considered to be valid;
#' the values for nitrogen N in the shapefile SoilAttr_BG_RO are in mg/100g, and therefore not comparable with N values of other countries;
#' the values for nitrogen N in the shapefile SoilAttr_ICELAND are in mg/kg, and therefore not comparable with N values of other countries;

#' These issues have been addressed in an updated version of the shapefiles: package-for-ESDAC-20190927.zip.

#' Panagos P., Van Liedekerke M., Jones A., Montanarella L., “European Soil Data Centre: Response to European policy support and public data requirements”; (2012) Land Use Policy, 29 (2), pp. 329-338. doi:10.1016/j.landusepol.2011.07.003
#' European Soil Data Centre (ESDAC), esdac.jrc.ec.europa.eu, European Commission, Joint Research Centre

##########