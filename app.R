#
# SDS 313 Shiny App Project 2 - Gathering Information about EVs in Washington
#

library(shiny)
library(tidyverse)

setwd("~/Desktop/RK - SDS 313/Project 2 App")

ev_data <- read.csv("Electric Vehicle Population Data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Rehaan Karnik - Project 2 - Washington EV Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      img(src = "https://depts.washington.edu/trac/wp-content/uploads/2024/08/EVChargingb.jpg", height = 150),
      p("Source: Washington State Transportation Center"),
      h6("Information from: https://catalog.data.gov/dataset/electric-vehicle-population-data"),
      
      
      #Select box for variable:
      selectInput("selectvar", label = h4("Choose a variable"), 
                  choices = list(
                    "Driving Range" = "Electric.Range",
                    "Base MSRP" = "Base.MSRP",
                    "Make" = "Make",
                    "Model" = "Model"), 
                  selected = "Electric.Range"),
      
      
      # Slider input for looking at a range of years
      sliderInput("year_filter", "Filter by Model Year:",
                  min = min(ev_data$Model.Year, na.rm = TRUE),
                  max = max(ev_data$Model.Year, na.rm = TRUE),
                  value = c(min(ev_data$Model.Year, na.rm = TRUE),
                            max(ev_data$Model.Year, na.rm = TRUE)), 
                  step = 1, sep = ''),
      
      #Plot controls for bins and color
      sliderInput("bins", "Number of bins:", min = 5, max = 60, value = 30),
      selectInput("plot_color", "Plot color:", choices = c("lightgreen", "cyan", "gold")),
      
      #Button to show Stats
      actionButton("show_stats", "Compute statistics")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("This app explores data about EVs registered in the state of Washington. Select a variable to view its distribution
        and use the slider to change the years you want to look at."),
      plotOutput("distPlot"),
      hr(),
      h3('Statistics:'),
      fluidRow(column(8, verbatimTextOutput("Statistics")))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered_data <- reactive({
    ev_data[ev_data$Model.Year >= input$year_filter[1] &
              ev_data$Model.Year <= input$year_filter[2] , ]
  })
  
  
  output$distPlot <- renderPlot({
    
    data <- filtered_data()
    var  <- input$selectvar
    
    #Numerical Visualizations
    if(var == "Electric.Range"){
      x <- data$Electric.Range; x <- x[!is.na(x) & x > 0]
      hist(x,
           breaks = input$bins, 
           main='Distribution of Driving Range of EVs',
           xlab='Miles',
           col = input$plot_color, border = 'darkgrey')
    }
    else if(var == "Base.MSRP"){
      x <- data$Base.MSRP; x <- x[!is.na(x) & x > 0]
      hist(x, 
           breaks = input$bins, 
           main='Distribution of Base MSRP (Base Prices)',
           xlab='USD',
           col = input$plot_color, border = 'darkgrey')
      
    }
    
    #Categorical Visualizations
    else if (var %in% c("Make","Model")) {
      
      #building counts to only keep top 10 car makes/models to keep readable
      topN <- 10
      counts <- data %>%
        filter(!is.na(.data[[var]])) %>%
        count(.data[[var]], sort = TRUE, name = "Freq") %>%
        slice_head(n = topN)
      
    
      
      if (var == "Make") {
        ggplot(counts, aes_string(x = var, y = "Freq")) +
          geom_col(fill = input$plot_color) +
          labs(title = "Distribution of Car Make",
               x = "Car Makes", y = "Count") + 
          theme_classic()
      }
      
      else if (var == "Model") {
        ggplot(counts, aes_string(x = var, y = "Freq")) +
          geom_col(fill = input$plot_color) +
          labs(title = "Distribution of Car Models",
               x = "Car Models", y = "Count") +
          theme_classic()
      }
      
    }
  })
  
  
  
  
  #Display median and 5-num summary if numerical variable selected
  output$Statistics <- renderPrint({ 
    req(input$show_stats)
      if (input$selectvar %in% c("Electric.Range","Base.MSRP")) {
        x <- filtered_data()[[input$selectvar]]
        x <- x[!is.na(x) & x > 0]
        cat("Median:\n")
        print(median(x))
        cat("\nFive-number summary (Min, Q1, Median, Q3, Max):\n")
        print(fivenum(x))
        }
      else if (input$selectvar %in% c("Make","Model")) {
        data   <- filtered_data()
        vec <- data[[input$selectvar]]
        vec <- vec[!is.na(vec)]
        
        tab <- sort(table(vec), decreasing = TRUE)
        cat("Counts (top 10):\n")
        print(head(tab, 10))
      }
    
      })
  

}


# Run the application 
shinyApp(ui = ui, server = server)