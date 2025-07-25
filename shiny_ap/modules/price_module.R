
price_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Price Analysis"),
    fluidRow(
      column(6, plotOutput(ns("price_hist"))),
      column(6, plotOutput(ns("price_boxplot")))
    ),
    fluidRow(
      column(12, h4("Average Price per Guest by City Category"), tableOutput(ns("avg_price_table")))
    )
  )
}

price_module_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$price_hist <- renderPlot({
      req(data_reactive())
      df <- data_reactive()

      ggplot(df, aes(x = `Price Per Guest`)) +
        geom_histogram(bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
        stat_bin(bins = 10, geom = "text", aes(label = ..count..), vjust = -0.5) +
        labs(title = "Distribution of Price per Guest", x = "Price per Guest ($)", y = "Frequency") +
        theme_minimal() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Add some space above bars for labels
    })

    output$price_boxplot <- renderPlot({
      req(data_reactive())
      df <- data_reactive()
      ggplot(df, aes(x = `City Category`, y = `Price Per Guest`)) +
        geom_boxplot(fill = "lightgreen") +
        labs(title = "Price per Guest by City Category", x = "City Category", y = "Price per Guest ($)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_cartesian(ylim = c(0, quantile(df$`Price Per Guest`, 0.95, na.rm = TRUE)))
    })

    output$avg_price_table <- renderTable({
      req(data_reactive())
      df <- data_reactive()
      df %>%
        group_by(`City Category`) %>%
        summarise(`Average price per guest in Drams` = round(mean(`Price Per Guest`, na.rm = TRUE), 2)) %>%
        arrange(desc(`Average price per guest in Drams`))
    }, striped = TRUE, hover = TRUE)

  })
}