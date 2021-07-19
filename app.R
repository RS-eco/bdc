# Load packages ----
library(shiny)
library(ggplot2)

# Load data ----
dat <- get(load("data/alt_bav_tk4tel.rda"))
load("data/bavaria.rda")
bavaria <- sf::st_transform(bavaria, sp::CRS("+init=epsg:31468"))

# User interface ----
ui <- fluidPage(
  titlePanel("Bavarian Data Cube"),
  sidebarLayout(
    sidebarPanel(
      #helpText(""),
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Altitude", "Aspect", "Slope", "Hill shade", "TRI",
                              "TPI", "TPI5", "Roughness", "Flow direction"),
                  selected = "Altitude")#,
      #sliderInput("lat_range", label = "Latitude of interest:",
      #            min = 9, max = 14, value = c(9, 14)),
      #sliderInput("long_range", label = "Longitude of interest:",
      #            min = 47, max = 51, value = c(47, 51))
    ),
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Altitude" = "altitude",
                   "Aspect"= "aspect", 
                   "Slope"= "slope", 
                   "Hill shade"= "hillshade", 
                   "TRI"= "tri",
                   "TPI"= "tpi", 
                   "TPI5"= "tpi5", 
                   "Roughness" = "roughness",
                   "Flow direction" = "flowdir")
    
    col_val <- switch(input$var,
                      "Altitude" = scales::rescale(unique(c(seq(min(dat$altitude), median(dat$altitude), length=5),
                                                            seq(median(dat$altitude), max(dat$altitude), length=5)))),
                      "Aspect"= scales::rescale(unique(c(seq(min(dat$aspect), median(dat$aspect), length=5),
                                                         seq(median(dat$aspect), max(dat$aspect), length=5)))), 
                      "Slope"= scales::rescale(unique(c(seq(min(dat$slope), median(dat$slope), length=5),
                                                        seq(median(dat$slope), max(dat$slope), length=5)))), 
                      "Hill shade"= scales::rescale(unique(c(seq(min(dat$hillshade), median(dat$hillshade), length=5),
                                                             seq(median(dat$hillshade), max(dat$hillshade), length=5)))), 
                      "TRI"= scales::rescale(unique(c(seq(min(dat$tri), median(dat$tri), length=5),
                                                      seq(median(dat$tri), max(dat$tri), length=5)))),
                      "TPI"= scales::rescale(unique(c(seq(min(dat$tpi), median(dat$tpi), length=5),
                                                      seq(median(dat$tpi), max(dat$tpi), length=5)))), 
                      "TPI5"= scales::rescale(unique(c(seq(min(dat$tpi5), median(dat$tpi5), length=5),
                                                       seq(median(dat$tpi5), max(dat$tpi5), length=5)))), 
                      "Roughness" = scales::rescale(unique(c(seq(min(dat$roughness), median(dat$roughness), length=5),
                                                             seq(median(dat$roughness), max(dat$roughness), length=5)))),
                      "Flow direction" = scales::rescale(unique(c(seq(min(dat$flowdir), median(dat$flowdir), length=5),
                                                                  seq(median(dat$flowdir), max(dat$flowdir), length=5)))))
    legend <- switch(input$var,
                     "Altitude" = "altitude",
                     "Aspect"="aspect", 
                     "Slope"="slope", 
                     "Hill shade"="hillshade", 
                     "TRI"="tri",
                     "TPI"="tpi", 
                     "TPI5"="tpi5", 
                     "Roughness" = "roughness",
                     "Flow direction" = "flowdir")
    
    ggplot() +
      geom_tile(data=dat, aes_string(x="x", y="y", fill=data)) +
      scale_fill_gradientn(name=legend, colours=terrain.colors(255),
                           na.value= "grey50", values=col_val) +
      geom_sf(data=sf::st_as_sf(bavaria), fill="transparent", col="black") +
      theme_bw() + labs(x="Longitude", y="Latitude") +
      coord_sf()
  })
}

# Run app ----
shinyApp(ui, server)
