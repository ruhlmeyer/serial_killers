library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(tm)
library(wordcloud2)

# Read in cleaned data
df <- read_csv('data/serial_killers_wiki_cleaned.csv', show_col_types = FALSE)

# Shiny App
ui <- dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "Serial Killer Analysis"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
    menuItem("Map", tabName = "map", icon = icon("map"))
  )),
  dashboardBody(
    tabItems(
      
      ### Home Page
      tabItem(
        tabName = "home",
        h2("Welcome to the Serial Killer Analysis Project")
      ),
      
      ### Dashboard Page
      tabItem(
        tabName = "dashboard",
        
        # Filters at the top
        fluidRow(
          box(
            title = "Filters",
            solidHeader = TRUE,
            status = "primary", 
            fluidRow(
              column(
                4,
                selectInput("country_filter", label = "Select Countries", 
                            choices = unique(df$Country), 
                            selected = unique(df$Country), 
                            multiple = TRUE)
              ),
              column(
                4,
                sliderInput("date_range", "Select Date Range",
                            min = as.numeric(format(min(df$Start, na.rm = TRUE), "%Y")),
                            max = as.numeric(format(max(df$End, na.rm = TRUE), "%Y")), 
                            value = c(as.numeric(format(min(df$Start, na.rm = TRUE), "%Y")), 
                                      as.numeric(format(max(df$End, na.rm = TRUE), "%Y"))), 
                            step = 1)
              ),
              column(
                4,
                sliderInput("victim_count", "Proven Victims", 
                            min = min(df$`Proven victims`, na.rm = TRUE), 
                            max = max(df$`Proven victims`, na.rm = TRUE), 
                            value = c(min(df$`Proven victims`, na.rm = TRUE), 
                                      max(df$`Proven victims`, na.rm = TRUE)))
              )
            ),
            width = 12,
            collapsible = TRUE
          )
        ),
        
        # 2x2 Grid of Visuals
        fluidRow(
          box(
            title = "Proven Victims by Country",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            plotlyOutput("victimChart")
          ),
          box(
            title = "Proven vs Possible Victims",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            plotlyOutput("victimComparison")
          )
        )
      ),
      
      ### Map Page
      tabItem(
        tabName = "map",
        h2("Map Page")
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Apply filters to all visuals
  df_filtered <- reactive({
    df %>%
      filter(Country %in% input$country_filter, 
             Start >= as.Date(paste0(input$date_range[1], "-01-01")), 
             End <= as.Date(paste0(input$date_range[2], "-12-31")),
             `Proven victims` >= input$victim_count[1], 
             `Proven victims` <= input$victim_count[2])
  })
  
  # Proven Victims by Country (Plotly)
  output$victimChart <- renderPlotly({
    filtered_data <- df_filtered()
    
    p1 <- ggplot(filtered_data, aes(x = reorder(Country, `Proven victims`, FUN = max), y = `Proven victims`)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      coord_flip() +  # Rotate the chart
      labs(title = "Proven Victims by Country", x = "Proven Victims", y = "Country")
    
    ggplotly(p1)
  })
  
  # Proven vs Possible Victims scatter plot (Plotly)
  output$victimComparison <- renderPlotly({
    filtered_data <- df_filtered()
    
    p2 <- ggplot(filtered_data, aes(x = `Proven victims`, y = `Possible victims`, text = `Name`)) +
      geom_point(color = "red", size = 1) +
      theme_minimal() +
      labs(title = "Proven vs Possible Victims", x = "Proven Victims", y = "Possible Victims")
    
    ggplotly(p2, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
