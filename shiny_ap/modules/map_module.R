
map_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Airbnb Listings Map"),
    leaflet::leafletOutput(ns("map")),
    br(),
    h4("Selected Listing Details:"),
    verbatimTextOutput(ns("selected_details"))
  )
}

map_module_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$map <- leaflet::renderLeaflet({
      req(data_reactive())
      df <- data_reactive()

      df <- df[!is.na(as.numeric(df$Latitude)) & !is.na(as.numeric(df$Longitude)), ]
      df$Latitude <- as.numeric(df$Latitude)
      df$Longitude <- as.numeric(df$Longitude)

      pal <- colorNumeric(
        palette = "viridis",
        domain = df$`Price Per Guest`
      )

      leaflet::leaflet(df) %>%
        setView(lng = 44.5167, lat = 40.1833, zoom = 12) %>%
        addTiles() %>%
        addCircleMarkers(
          ~Longitude, ~Latitude,
          radius = ~sqrt(`Max Guests`),
          color = ~pal(`Price Per Guest`),
          fillOpacity = 0.7,
          stroke = FALSE,
          layerId = ~`Listing ID`,
          popup = ~paste0(
            "<b>", `Listing Name`, "</b><br/>",
            "Price/Guest: $", round(`Price Per Guest`, 2), "<br/>",
            "Rating: ", `Overall Rating`, "<br/>",
            "Guests: ", `Max Guests`
          ),
          label = ~`Listing Name`
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = ~`Price Per Guest`,
          title = "Price per Guest ($)",
          opacity = 1
        )
    })

    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (is.null(click)) return()

      selected_listing <- data_reactive() %>%
        dplyr::filter(`Listing ID` == click$id)


      output$selected_details <- renderPrint({
        if (nrow(selected_listing) > 0) {
          selected_listing %>%
            dplyr::select(
              `Listing Name`,
              `Price Per Guest`,
              `Overall Rating`,
              `Max Guests`,
              `Bedrooms`,
              `Bathrooms`,
              `City Category`
            ) %>%
            as.list()
        } else {
          "No data for selected listing."
        }
      })
    })
  })
}