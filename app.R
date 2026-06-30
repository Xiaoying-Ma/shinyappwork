library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

# Read data
house_raw <- read.csv("https://raw.githubusercontent.com/Xiaoying-Ma/shinyappwork/main/NY-House-Dataset.csv")

# Save original total listings
raw_total <- nrow(house_raw)

# Clean data and create variables
house <- house_raw %>%
  filter(!is.na(PRICE),
         !is.na(PROPERTYSQFT),
         !is.na(TYPE),
         !is.na(BEDS),
         !is.na(BATH),
         !is.na(LATITUDE),
         !is.na(LONGITUDE),
         PRICE > 0,
         PROPERTYSQFT > 0,
         PRICE <= 20000000,
         PROPERTYSQFT <= 20000,
         BEDS <= 10,
         BATH <= 10) %>%
  mutate(
    PRICE_M = PRICE / 1000000,
    LOG_PRICE = log10(PRICE),
    PRICE_PER_SQFT = PRICE / PROPERTYSQFT
  )

# Color palette
dark_purple <- "#6B2F63"
mid_purple <- "#8E5A8A"
deep_blue <- "#24476B"
mid_blue <- "#5B8DB8"
light_blue <- "#CFE8FF"
soft_blue <- "#A7C7E7"
soft_green <- "#B7E4C7"
soft_purple <- "#D8BFD8"
soft_orange <- "#FFD6A5"

# Header
header0 <- dashboardHeader(
  title = "NY Housing",
  tags$li(
    class = "dropdown",
    tags$a(href = "https://github.com/Xiaoying-Ma/shinyappwork",
           target = "_blank",
           icon("code"), " Source Code")
  ),
  tags$li(
    class = "dropdown",
    tags$a(href = "#",
           target = "_blank",
           icon("file-alt"), " Report")
  ),
  tags$li(
    class = "dropdown",
    tags$a(href = "https://raw.githubusercontent.com/Xiaoying-Ma/shinyappwork/main/NY-House-Dataset.csv",
           target = "_blank",
           icon("database"), " Data Source")
  )
)

# Sidebar
sider0 <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-column")),
    hr(),
    
    sliderInput("priceRange", "Price Range (Million USD)",
                min = 0,
                max = ceiling(max(house$PRICE_M, na.rm = TRUE)),
                value = c(0, ceiling(max(house$PRICE_M, na.rm = TRUE))),
                step = 1),
    
    selectInput("typeInput", "House Type",
                choices = c("All", sort(unique(house$TYPE))),
                selected = "All"),
    
    selectInput("localityInput", "Locality",
                choices = c("All", sort(unique(house$LOCALITY))),
                selected = "All"),
    
    sliderInput("bedRange", "Bedrooms",
                min = 0, max = 10,
                value = c(0, 10),
                step = 1),
    
    sliderInput("bathRange", "Bathrooms",
                min = 0, max = 10,
                value = c(0, 10),
                step = 1),
    
    sliderInput("sqftRange", "Property Sqft",
                min = 0,
                max = ceiling(max(house$PROPERTYSQFT, na.rm = TRUE)),
                value = c(0, ceiling(max(house$PROPERTYSQFT, na.rm = TRUE))),
                step = 500)
  )
)

# Body
body0 <- dashboardBody(
  
  tags$head(
    tags$style(HTML("
      .skin-blue .main-header .logo {
        background-color: #6B2F63;
        font-weight: bold;
      }
      .skin-blue .main-header .navbar {
        background-color: #6B2F63;
      }
      .skin-blue .main-sidebar {
        background-color: #2F2430;
      }
      .skin-blue .main-sidebar .sidebar-menu > li.active > a {
        background-color: #8E5A8A;
      }
      .content-wrapper {
        background-color: #F7F3F7;
      }
      .box.box-solid.box-primary > .box-header {
        background-color: #6B2F63;
        color: white;
      }
      .box.box-solid.box-primary {
        border: 1px solid #6B2F63;
      }
    "))
  ),
  
  fluidRow(
    valueBoxOutput("totalListings", width = 3),
    valueBoxOutput("avgPrice", width = 3),
    valueBoxOutput("avgSqft", width = 3),
    valueBoxOutput("avgPriceSqft", width = 3)
  ),
  
  fluidRow(
    box(title = "Housing Location Map",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("mapPlot", height = 350)),
    
    box(title = "Average Price by Locality",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("localityPlot", height = 350))
  ),
  
  fluidRow(
    box(title = "Log Price vs Property Size",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("scatterPlot", height = 350)),
    
    box(title = "Log Price by House Type",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("boxPlot", height = 350))
  )
)

# UI
ui <- dashboardPage(
  header = header0,
  sidebar = sider0,
  body = body0
)

# Server
server <- function(input, output) {
  
  # Filtered data
  filtered_house <- reactive({
    
    data_use <- house %>%
      filter(
        PRICE_M >= input$priceRange[1],
        PRICE_M <= input$priceRange[2],
        BEDS >= input$bedRange[1],
        BEDS <= input$bedRange[2],
        BATH >= input$bathRange[1],
        BATH <= input$bathRange[2],
        PROPERTYSQFT >= input$sqftRange[1],
        PROPERTYSQFT <= input$sqftRange[2]
      )
    
    if (input$typeInput != "All") {
      data_use <- data_use %>% filter(TYPE == input$typeInput)
    }
    
    if (input$localityInput != "All") {
      data_use <- data_use %>% filter(LOCALITY == input$localityInput)
    }
    
    data_use
  })
  
  output$totalListings <- renderValueBox({
    valueBox(comma(raw_total),
             "Total Listings",
             icon = icon("house"),
             color = "purple")
  })
  
  output$avgPrice <- renderValueBox({
    valueBox(paste0("$", round(mean(filtered_house()$PRICE_M, na.rm = TRUE), 2), "M"),
             "Average Price",
             icon = icon("dollar-sign"),
             color = "blue")
  })
  
  output$avgSqft <- renderValueBox({
    valueBox(comma(round(mean(filtered_house()$PROPERTYSQFT, na.rm = TRUE), 0)),
             "Average Sqft",
             icon = icon("ruler-combined"),
             color = "green")
  })
  
  output$avgPriceSqft <- renderValueBox({
    valueBox(dollar(round(mean(filtered_house()$PRICE_PER_SQFT, na.rm = TRUE), 0)),
             "Average Price per Sqft",
             icon = icon("calculator"),
             color = "yellow")
  })
  
  # Plotly map: darker blue means higher price
  output$mapPlot <- renderPlotly({
    
    plot_data <- filtered_house()
    
    plot_ly(
      data = plot_data,
      x = ~LONGITUDE,
      y = ~LATITUDE,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 4,
        color = ~PRICE_M,
        colorscale = list(
          c(0, light_blue),
          c(0.5, mid_blue),
          c(1, deep_blue)
        ),
        showscale = TRUE,
        colorbar = list(
          title = "Darker = Higher Price",
          len = 0.45,
          thickness = 10
        ),
        opacity = 0.75
      ),
      text = ~paste0(
        "Price: $", comma(PRICE),
        "<br>Type: ", TYPE,
        "<br>Beds: ", BEDS,
        "<br>Bath: ", BATH,
        "<br>Sqft: ", comma(PROPERTYSQFT),
        "<br>Address: ", ADDRESS
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Housing Location Map",
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
  })
  
  # Average price by locality
  output$localityPlot <- renderPlotly({
    
    plot_data <- filtered_house() %>%
      filter(!is.na(LOCALITY), LOCALITY != "") %>%
      group_by(LOCALITY) %>%
      summarise(
        avg_price_m = mean(PRICE_M, na.rm = TRUE),
        listings = n(),
        .groups = "drop"
      ) %>%
      filter(listings >= 5) %>%
      arrange(desc(avg_price_m)) %>%
      head(10)
    
    p <- ggplot(plot_data,
                aes(x = reorder(LOCALITY, avg_price_m),
                    y = avg_price_m,
                    text = paste0(
                      "Locality: ", LOCALITY,
                      "<br>Average Price: $", round(avg_price_m, 2), "M",
                      "<br>Listings: ", listings
                    ))) +
      geom_col(fill = deep_blue) +
      coord_flip() +
      labs(title = "Top Localities by Average Price",
           x = "Locality",
           y = "Average Price (Million USD)") +
      scale_y_continuous(labels = label_dollar(suffix = "M")) +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Scatter plot with linear regression line
  output$scatterPlot <- renderPlotly({
    
    plot_data <- filtered_house()
    
    lm_fit <- lm(LOG_PRICE ~ PROPERTYSQFT, data = plot_data)
    
    line_data <- data.frame(
      PROPERTYSQFT = seq(
        min(plot_data$PROPERTYSQFT, na.rm = TRUE),
        max(plot_data$PROPERTYSQFT, na.rm = TRUE),
        length.out = 100
      )
    )
    
    line_data$LOG_PRICE <- predict(lm_fit, newdata = line_data)
    
    p <- ggplot() +
      geom_point(
        data = plot_data,
        aes(x = PROPERTYSQFT,
            y = LOG_PRICE,
            text = paste0(
              "Price: $", comma(PRICE),
              "<br>Log Price: ", round(LOG_PRICE, 2),
              "<br>Sqft: ", comma(PROPERTYSQFT),
              "<br>Type: ", TYPE,
              "<br>Beds: ", BEDS,
              "<br>Bath: ", BATH
            )),
        color = mid_blue,
        alpha = 0.55,
        size = 1.3
      ) +
      geom_line(
        data = line_data,
        aes(x = PROPERTYSQFT, y = LOG_PRICE),
        color = dark_purple,
        linewidth = 1.4
      ) +
      labs(title = "Log Price vs Property Size",
           x = "Property Square Feet",
           y = "Log Price") +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Boxplot by house type
  output$boxPlot <- renderPlotly({
    
    top_types <- filtered_house() %>%
      count(TYPE, sort = TRUE) %>%
      head(7) %>%
      pull(TYPE)
    
    p <- filtered_house() %>%
      filter(TYPE %in% top_types) %>%
      ggplot(aes(x = reorder(TYPE, LOG_PRICE, median),
                 y = LOG_PRICE,
                 fill = TYPE,
                 text = paste0(
                   "Type: ", TYPE,
                   "<br>Price: $", comma(PRICE),
                   "<br>Log Price: ", round(LOG_PRICE, 2)
                 ))) +
      geom_boxplot(
        color = deep_blue,
        outlier.color = mid_purple,
        outlier.size = 0.6,
        outlier.alpha = 0.45,
        alpha = 0.85
      ) +
      labs(title = "Log Price Distribution by House Type",
           x = "House Type",
           y = "Log Price") +
      scale_fill_manual(values = c(
        "#A7C7E7",
        "#D8BFD8",
        "#B7E4C7",
        "#FFD6A5",
        "#E6D5F7",
        "#CDEAC0",
        "#CFE8FF"
      )) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)