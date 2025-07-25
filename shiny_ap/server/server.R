# server.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(RColorBrewer)


listings_data <- read.csv("cleaned_rental_data_from_Airbnb_v2.csv", na.strings = c("", "NA"))


listings_data$Is.Superhost <- as.logical(ifelse(listings_data$Is.Superhost == "TRUE", TRUE, FALSE))
listings_data$Is.Instant.Bookable <- as.logical(ifelse(listings_data$Is.Instant.Bookable == "TRUE", TRUE, FALSE))


listings_data$Estimated_Monthly_Rent <- listings_data$TTM.Avg.Daily.Rate * 30


server <- function(input, output, session) {


  filtered_data <- reactive({
    req(input$city_category, input$price_range, input$property_types, input$room_types) # Ensure inputs are initialized

    data <- listings_data

    if (input$city_category != "All") {
      data <- data[data$City.Category == input$city_category, ]
    }

    data <- data[data$Price.Per.Guest >= input$price_range[1] & data$Price.Per.Guest <= input$price_range[2], ]

    if (!is.null(input$property_types) && length(input$property_types) > 0) {
      data <- data[data$Property.Type %in% input$property_types, ]
    }

    if (!is.null(input$room_types) && length(input$room_types) > 0) {
      data <- data[data$Room.Type %in% input$room_types, ]
    }

    if (input$superhost_only) {
      data <- data[data$Is.Superhost == TRUE, ]
    }

    data <- data[!is.na(data$Latitude) & !is.na(data$Longitude), ]

    return(data)
  })

  observe({
    property_choices <- sort(unique(listings_data$Property.Type))
    property_choices <- property_choices[!is.na(property_choices)]
    updateCheckboxGroupInput(session, "property_types",
                             choices = property_choices,
                             selected = property_choices # Select all by default
    )

    # Update Room Type choices
    room_choices <- sort(unique(listings_data$Room.Type))
    # Remove NA if present
    room_choices <- room_choices[!is.na(room_choices)]
    updateCheckboxGroupInput(session, "room_types",
                             choices = room_choices,
                             selected = room_choices # Select all by default
    )
  })

  # --- Map Tab ---
  output$rental_map <- renderLeaflet({
    data <- filtered_data()

    if (nrow(data) == 0) {
      leaflet() %>% addTiles() %>% setView(lng = 44.5, lat = 40.18, zoom = 12) %>%
        addLabelOnlyMarkers(lng = 44.5, lat = 40.18, label = "No listings match the current filters.")
    } else {
      # Create color palette for price
      pal <- colorNumeric("viridis", domain = data$Price.Per.Guest, na.color = "transparent")

      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          radius = ~sqrt(Price.Per.Guest) / 2, # Adjust size scaling
          color = ~pal(Price.Per.Guest),
          fillOpacity = 0.7,
          stroke = FALSE,
          label = ~paste0(Listing.Name, "<br/>Price/Guest: $", Price.Per.Guest),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          ),
          clusterOptions = markerClusterOptions()
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Price.Per.Guest,
                  title = "Price per Guest ($)",
                  opacity = 1)
    }
  })

  # --- Rent Analysis Tab ---
  # Value Boxes
  output$avg_daily_rate <- renderValueBox({
    data <- filtered_data()
    if (nrow(data) == 0) {
      avg_rate <- 0
    } else {
      avg_rate <- round(mean(data$TTM.Avg.Daily.Rate, na.rm = TRUE), 2)
    }
    valueBox(
      value = paste0("$", avg_rate),
      subtitle = "Avg TTM Daily Rate",
      icon = icon("dollar-sign"),
      color = "light-blue"
    )
  })

  output$avg_monthly_rent <- renderValueBox({
    data <- filtered_data()
    if (nrow(data) == 0) {
      avg_monthly <- 0
    } else {
      # Use the pre-calculated monthly rent
      avg_monthly <- round(mean(data$Estimated_Monthly_Rent, na.rm = TRUE), 2)
    }
    valueBox(
      value = paste0("$", avg_monthly),
      subtitle = "Est. Avg Monthly Rent (TTM)",
      icon = icon("calendar-alt"),
      color = "olive"
    )
  })

  output$avg_occupancy <- renderValueBox({
    data <- filtered_data()
    if (nrow(data) == 0) {
      avg_occ <- 0
    } else {
      avg_occ <- round(mean(data$TTM.Occupancy.Rate, na.rm = TRUE) * 100, 2) # Convert to %
    }
    valueBox(
      value = paste0(avg_occ, "%"),
      subtitle = "Avg TTM Occupancy Rate",
      icon = icon("percentage"),
      color = "teal"
    )
  })

  # Plots for Rent Analysis Tab
  output$price_histogram <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    ggplot(data, aes(x = TTM.Avg.Daily.Rate)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black") +
      labs(x = "TTM Avg Daily Rate ($)", y = "Frequency") +
      theme_minimal()
  })

  output$price_location_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Location.Rating))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    ggplot(data, aes(x = as.factor(Location.Rating), y = TTM.Avg.Daily.Rate)) +
      geom_boxplot(fill = "lightgreen") +
      labs(x = "Location Rating", y = "TTM Avg Daily Rate ($)") +
      theme_minimal()
  })

  output$price_property_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Property.Type))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    data_summary <- data %>%
      group_by(Property.Type) %>%
      summarise(Avg_Rate = mean(TTM.Avg.Daily.Rate, na.rm = TRUE), .groups = 'drop')

    ggplot(data_summary, aes(x = reorder(Property.Type, Avg_Rate), y = Avg_Rate, fill = Property.Type)) +
      geom_col() +
      coord_flip() +
      labs(x = "Property Type", y = "Average TTM Daily Rate ($)") +
      theme_minimal() + guides(fill = "none")
  })

  output$price_room_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Room.Type))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    data_summary <- data %>%
      group_by(Room.Type) %>%
      summarise(Avg_Rate = mean(TTM.Avg.Daily.Rate, na.rm = TRUE), .groups = 'drop')

    ggplot(data_summary, aes(x = reorder(Room.Type, Avg_Rate), y = Avg_Rate, fill = Room.Type)) +
      geom_col() +
      coord_flip() +
      labs(x = "Room Type", y = "Average TTM Daily Rate ($)") +
      theme_minimal() + guides(fill = "none")
  })

  # --- Property Insights Tab ---
  output$property_type_bar <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Property.Type))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    ggplot(data, aes(x = Property.Type, fill = Property.Type)) +
      geom_bar() +
      labs(x = "Property Type", y = "Number of Listings") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = "none")
  })

  output$room_type_bar <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Room.Type))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    ggplot(data, aes(x = Room.Type, fill = Room.Type)) +
      geom_bar() +
      labs(x = "Room Type", y = "Number of Listings") +
      theme_minimal() + guides(fill = "none")
  })

  output$rating_property_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Property.Type)) || any(is.na(data$Overall.Rating))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    data_summary <- data %>%
      group_by(Property.Type) %>%
      summarise(Avg_Rating = mean(Overall.Rating, na.rm = TRUE), .groups = 'drop')

    ggplot(data_summary, aes(x = reorder(Property.Type, Avg_Rating), y = Avg_Rating, fill = Property.Type)) +
      geom_col() +
      coord_flip() +
      labs(x = "Property Type", y = "Average Overall Rating") +
      theme_minimal() + guides(fill = "none")
  })

  output$rating_room_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Room.Type)) || any(is.na(data$Overall.Rating))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    data_summary <- data %>%
      group_by(Room.Type) %>%
      summarise(Avg_Rating = mean(Overall.Rating, na.rm = TRUE), .groups = 'drop')

    ggplot(data_summary, aes(x = reorder(Room.Type, Avg_Rating), y = Avg_Rating, fill = Room.Type)) +
      geom_col() +
      coord_flip() +
      labs(x = "Room Type", y = "Average Overall Rating") +
      theme_minimal() + guides(fill = "none")
  })

  output$beds_price_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Beds)) || any(is.na(data$Price.Per.Guest))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    ggplot(data, aes(x = as.factor(Beds), y = Price.Per.Guest)) +
      geom_boxplot(fill = "coral") +
      labs(x = "Number of Beds", y = "Price Per Guest ($)") +
      theme_minimal()
  })

  output$baths_price_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0 || any(is.na(data$Bathrooms)) || any(is.na(data$Price.Per.Guest))) return(ggplot() + annotate("text", x=0, y=0, label="No data") + theme_void())
    # Handle potential non-integer bathrooms if needed (e.g., 1.5)
    ggplot(data, aes(x = as.factor(Bathrooms), y = Price.Per.Guest)) +
      geom_boxplot(fill = "gold") +
      labs(x = "Number of Bathrooms", y = "Price Per Guest ($)") +
      theme_minimal()
  })

  output$listings_table <- renderDT({
    data <- filtered_data()
    display_cols <- c("Listing.Name", "Property.Type", "Room.Type", "Is.Superhost",
                      "Max.Guests", "Bedrooms", "Beds", "Bathrooms",
                      "TTM.Avg.Daily.Rate", "TTM.Occupancy.Rate", "Price.Per.Guest", "City.Category")
    datatable(data[, display_cols, drop = FALSE], options = list(scrollX = TRUE))
  })
}