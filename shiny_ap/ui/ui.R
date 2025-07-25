
library(shiny)
library(leaflet)

source("modules/map_module.R")
source("modules/price_module.R")
source("modules/quality_module.R")

app_ui <- fluidPage(
  titlePanel("Armeinna Cost of Living Alanlysis"),

  tabsetPanel(
    tabPanel("Map View",
             map_module_ui("map_mod")
    ),
    tabPanel("Price Analysis",
             price_module_ui("price_mod")
    ),
    tabPanel("Quality Analysis",
             quality_module_ui("quality_mod")
    )
  )
)
