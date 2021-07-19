#' ## Create SpatialPolygonsDataFrame of Bavaria

#' Get GADM data of Germany
deu <- raster::getData("GADM", country="DEU", level=1, path="/media/matt/Data/Documents/Wissenschaft/Data/GADM")

#' Subset data by NAME_1
bavaria <- deu[deu$NAME_1 == "Bayern",]
sp::plot(bavaria)

#' Save to file
bavaria <- sf::st_as_sf(bavaria)
plot(bavaria)
save(bavaria, file="data/bavaria.rda", compress="xz")
