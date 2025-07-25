
quality_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Quality of Life / Ratings Analysis"),

    fluidRow(
      column(6, plotOutput(ns("rating_hist"))),
      column(6, plotOutput(ns("rating_boxplot")))
    ),
    fluidRow(
      column(12, h4("Average Ratings by City Category"), tableOutput(ns("avg_rating_table")))
    ),


    hr(),
    h3("Price Analysis by Multiple Factors"),

    fluidRow(
      column(6, plotOutput(ns("price_by_property_type"))),
      column(6, plotOutput(ns("price_by_superhost")))
    ),
    fluidRow(
      column(6, plotOutput(ns("price_by_instant_book"))),
      column(6, plotOutput(ns("price_by_cancellation")))
    ),

    hr(),
    h3("Value Analysis (Rating per Unit Price)"),
    p("Listings with higher 'Value Score' offer better ratings relative to their price."),

    fluidRow(
      column(6, plotOutput(ns("value_score_histogram"))),
      column(6, plotOutput(ns("value_score_boxplot")))
    ),
    fluidRow(
      column(12, h4("Top Value Listings (Best Rating/Price Ratio)"), tableOutput(ns("top_value_table")))
    )
  )
}


quality_module_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$rating_hist <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_plot <- df %>% filter(!is.na(`Overall Rating`))
      ggplot(df_plot, aes(x = `Overall Rating`)) +
        geom_histogram(bins = 20, fill = "salmon", color = "black") +
        labs(title = "Distribution of Overall Ratings", x = "Overall Rating", y = "Frequency") +
        theme_minimal()
    })

    output$rating_boxplot <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_plot <- df %>% filter(!is.na(`Overall Rating`))
      ggplot(df_plot, aes(x = `City Category`, y = `Overall Rating`)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Overall Rating by City Category", x = "City Category", y = "Overall Rating") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$avg_rating_table <- renderTable({
      req(data_reactive())
      df <- data_reactive()
      df %>%
        filter(!is.na(`Overall Rating`)) %>%
        group_by(`City Category`) %>%
        summarise(`Rating given by visitors` = round(mean(`Overall Rating`, na.rm = TRUE), 2)) %>%
        arrange(desc(`Rating given by visitors`))
    }, striped = TRUE, hover = TRUE)


    output$price_by_property_type <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_plot <- df %>% filter(!is.na(`Price Per Guest`))


      top_property_types <- df_plot %>%
        count(`Property Type`, sort = TRUE) %>%
        head(10) %>%
        pull(`Property Type`)

      df_plot_filtered <- df_plot %>%
        filter(`Property Type` %in% top_property_types)

      ggplot(df_plot_filtered, aes(x = reorder(`Property Type`, `Price Per Guest`, median, na.rm = TRUE), y = `Price Per Guest`)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = "Price Per Guest by Top Property Types", x = "Property Type", y = "Price Per Guest ($)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_cartesian(ylim = c(0, quantile(df_plot_filtered$`Price Per Guest`, 0.95, na.rm = TRUE)))  # Limit y-axis to 95th percentile
    })


    output$price_by_superhost <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_plot <- df %>% filter(!is.na(`Price Per Guest`) & !is.na(`Is Superhost`))

      ggplot(df_plot, aes(x = factor(`Is Superhost`), y = `Price Per Guest`)) +
        geom_boxplot(fill = "lightgreen") +
        labs(title = "Price Per Guest: Superhost vs Non-Superhost", x = "Is Superhost", y = "Price Per Guest ($)") +
        theme_minimal() +
        coord_cartesian(ylim = c(0, quantile(df_plot$`Price Per Guest`, 0.95, na.rm = TRUE)))  # Limit y-axis to 95th percentile
    })


    output$price_by_instant_book <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_plot <- df %>% filter(!is.na(`Price Per Guest`) & !is.na(`Is Instant Bookable`))

      ggplot(df_plot, aes(x = factor(`Is Instant Bookable`), y = `Price Per Guest`)) +
        geom_boxplot(fill = "lightyellow") +
        labs(title = "Price Per Guest: Instant Bookable vs Not", x = "Is Instant Bookable", y = "Price Per Guest ($)") +
        theme_minimal() +
        coord_cartesian(ylim = c(0, quantile(df_plot$`Price Per Guest`, 0.95, na.rm = TRUE)))  # Limit y-axis to 95th percentile
    })

    # Price by Cancellation Policy
    output$price_by_cancellation <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_plot <- df %>% filter(!is.na(`Price Per Guest`) & !is.na(`Cancellation Policy`))

      ggplot(df_plot, aes(x = `Cancellation Policy`, y = `Price Per Guest`)) +
        geom_boxplot(fill = "lightpink") +
        labs(title = "Price Per Guest by Cancellation Policy", x = "Cancellation Policy", y = "Price Per Guest ($)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_cartesian(ylim = c(0, quantile(df_plot$`Price Per Guest`, 0.95, na.rm = TRUE)))  # Limit y-axis to 95th percentile
    })


    calculate_value_score <- function(df) {
      df %>%
        filter(!is.na(`Overall Rating`) & !is.na(`Price Per Guest`)) %>%
        mutate(

          `Value Score` = `Overall Rating` / (`Price Per Guest` + 0.01)
        )
    }


    output$value_score_histogram <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_value <- calculate_value_score(df)

      ggplot(df_value, aes(x = `Value Score`)) +
        geom_histogram(bins = 30, fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Distribution of Value Score (Rating/Price)",
             x = "Value Score (Rating per $)",
             y = "Frequency") +
        theme_minimal()
    })


    output$value_score_boxplot <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      df_value <- calculate_value_score(df)

      ggplot(df_value, aes(x = `City Category`, y = `Value Score`)) +
        geom_boxplot(fill = "lavender") +
        labs(title = "Value Score by City Category",
             x = "City Category",
             y = "Value Score (Rating per $)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$top_value_table <- renderTable({
      req(data_reactive())
      df <- data_reactive()
      df_value <- calculate_value_score(df)

      df_value %>%
        arrange(desc(`Value Score`)) %>%
        head(10) %>%
        select(
          `Listing Name`,
          `Overall Rating`,
          `Price Per Guest`,
          `Value Score`,
          `City Category`
        ) %>%
        rename(
          `Listing Name` = `Listing Name`,
          `Rating` = `Overall Rating`,
          `Price per Guest ($)` = `Price Per Guest`,
          `Value Score (Rating/$)` = `Value Score`,
          `City` = `City Category`
        )
    }, striped = TRUE, hover = TRUE)

  })
}