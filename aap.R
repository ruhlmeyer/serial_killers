library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(tm)
library(wordcloud2)
library(lubridate)  # For handling date ranges

# Read in cleaned data
df <- read_csv('data/serial_killers_wiki_cleaned.csv', show_col_types = FALSE)

# Convert relevant columns to numeric, if necessary
df$`Proven victims` <- as.numeric(df$`Proven victims`)
df$`Possible victims` <- as.numeric(df$`Possible victims`)

# Ensure Start and End columns are correctly formatted as dates
df$Start <- as.Date(df$Start, format = "%Y-%m-%d")
df$End <- as.Date(df$End, format = "%Y-%m-%d")

# Handle NA values if necessary
df <- df %>%
  filter(!is.na(Start), !is.na(End))  # Filtering out any rows with missing dates


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
    tags$head(
      tags$style(HTML("
        .box-body {
          padding: 0 !important;
        }
        .wordcloud-container {
          width: 100%;
          height: 100%;
        }
      "))
    ),
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
                selectizeInput("country_filter", label = "Select Countries", 
                               choices = unique(df$Country), 
                               selected = unique(df$Country),  # Set default to all countries
                               multiple = TRUE,
                               options = list(placeholder = 'Select countries',
                                              hideSelected = TRUE,  # Hides selected countries from the dropdown list
                                              plugins = list('remove_button')))
              ),
              column(
                2,
                actionButton("select_all", "Select All Countries", class = "btn-primary")
              ),
              column(
                2,
                actionButton("clear_all", "Clear All Countries", class = "btn-danger")  # Clear all button
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
            title = "Notes Word Cloud",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            height = 500,  # Set height to fit with 2x2 grid
            div(class = "wordcloud-container", wordcloud2Output("wordcloud", width = "100%", height = "400px"))
          ),
          box(
            title = "Overlapping Periods Heatmap",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            height = 500,  # Set height to fit with 2x2 grid
            plotlyOutput("overlapHeatmap", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Proven Victims by Country",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            height = 500,  # Height adjusted to fit with the grid
            plotlyOutput("victimChart", height = "400px")
          ),
          box(
            title = "Proven vs Possible Victims",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            height = 500,  # Height adjusted to fit with the grid
            plotlyOutput("victimComparison", height = "400px")
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
server <- function(input, output, session) {
  
  # Apply filters to all visuals
  df_filtered <- reactive({
    # If country_filter is empty or null, select all countries
    if (is.null(input$country_filter) || length(input$country_filter) == 0) {
      selected_countries <- unique(df$Country)
    } else {
      selected_countries <- input$country_filter
    }
    
    filtered_df <- df %>%
      filter(Country %in% selected_countries, 
             Start >= as.Date(paste0(input$date_range[1], "-01-01")), 
             End <= as.Date(paste0(input$date_range[2], "-12-31")),
             `Proven victims` >= input$victim_count[1], 
             `Proven victims` <= input$victim_count[2])
    
    return(filtered_df)
  })
  
  # Word cloud based on 'Notes' column
  output$wordcloud <- renderWordcloud2({
    # Create a text corpus and clean it
    corpus <- Corpus(VectorSource(df_filtered()$Notes))
    corpus_clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeWords, stopwords("en")) %>%
      tm_map(stripWhitespace)
    
    # Create term-document matrix
    tdm <- TermDocumentMatrix(corpus_clean)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    word_data <- data.frame(word = names(word_freqs), freq = word_freqs)
    
    # Generate word cloud
    wordcloud2(word_data)
  })
  
  # Proven Victims by Country (Plotly)
  output$victimChart <- renderPlotly({
    filtered_data <- df_filtered()
    
    p1 <- ggplot(filtered_data, aes(x = reorder(Country, `Proven victims`, FUN = max), y = `Proven victims`)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      coord_flip() +  # Rotate the chart
      theme(
        plot.margin = margin(t = 10, r = 10, b = 10, l = 50),  # Reduced margin for filling space
        axis.text.y = element_text(size = 8)  # Smaller font size for y-axis labels
      ) +
      labs(title = "Proven Victims by Country", x = "Proven Victims", y = "Country")
    
    ggplotly(p1) %>%
      layout(margin = list(l = 100, r = 50, b = 50, t = 50))  # Adjust layout for filling space
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
  
  # Overlapping periods heatmap
  output$overlapHeatmap <- renderPlotly({
    filtered_data <- df_filtered()
    
    # Ensure proper year extraction from the Start and End columns
    start_years <- year(filtered_data$Start)
    end_years <- year(filtered_data$End)
    
    # Create a date range from the minimum Start year to the maximum End year
    date_range <- seq(min(start_years), max(end_years), by = 1)
    
    # Initialize overlap count array
    overlap_counts <- rep(0, length(date_range))
    
    # Count overlaps for each year
    for (i in 1:nrow(filtered_data)) {
      start_year <- start_years[i]
      end_year <- end_years[i]
      overlap_counts[date_range >= start_year & date_range <= end_year] <- overlap_counts[date_range >= start_year & date_range <= end_year] + 1
    }
    
    # Create Plotly bar chart
    plot_ly(
      x = ~date_range,
      y = ~overlap_counts,
      type = 'bar',
      marker = list(color = 'skyblue')
    ) %>%
      layout(
        title = "Overlapping Periods Heatmap",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Overlaps"),
        margin = list(l = 50, r = 30, b = 50, t = 50)
      )
  })
  
  # Select all countries when button is clicked
  observeEvent(input$select_all, {
    updateSelectizeInput(session, "country_filter", selected = unique(df$Country))
    session$sendCustomMessage(type = "select-all-countries", message = unique(df$Country))
  })
  
  # Clear all countries when button is clicked
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "country_filter", selected = NULL)  # Clears all selected countries
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
