#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)

# Load Data---------
crimes <- data.frame(read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv"))

#------------------------------------

header <- dashboardHeader(title = "NYS Crime Data")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    menuItem("Third Thing?", icon("plot"), tabName = "Third tab"),
  
    #Select Imputs---------------------------------------------
  selectInput(inputId = "county",
              label = "Select County",
              choices = c("ALBANY","ALLEGANY","BRONX","BROOME","CATTARAUGUS", "CAYUGA",
                          "CHAUTAUQUA", "CHEMUNG", "CHENANGO", "CLINTON", "COLUMBIA","CORTLAND",
                          "DELAWARE", "DUTCHESS", "ERIE", "ESSEX", "FRANKLIN", "FULTON",
                          "GENESEE", "GREENE", "HERKIMER", "JEFFERSON", "KINGS",  "LEWIS",
                          "LIVINGSTON", "MADISON", "MONROE", "MONTGOMERY", "NASSAU", "NEW YORK",
                          "NIAGARA", "ONEIDA", "ONONDAGA", "ONTARIO", "ORANGE", "ORLEANS",
                          "OSWEGO", "OTSEGO", "PUTNAM", "QUEENS", "RENSSELAER", "RICHMOND",
                          "ROCKLAND", "SARATOGA", "SCHENECTADY", "SCHOHARIE", "SCHUYLER", "SENECA",
                          "ST LAWRENCE", "STEUBEN", "SUFFOLK", "SULLIVAN","TIOGA","TOMPKINS",
                          "ULSTER", "WARREN", "WASHINGTON", "WAYNE", "WESTCHESTER", "WYOMING")),
  
  # Select Date Range for Filter by year---------------------------

  sliderInput(inputId = "year_range", label = "Year Range:", 
              min = 1990, max = 2019, value = 1990, ticks = TRUE, sep = "", 
              animate = animationOptions(interval = 500)) # Animates over time--Not sure I want this? 
  
  )
)

body <- dashboardBody(tabItems(
  tabItem("table",
          fluidPage(box(title = "County", DT::dataTableOutput("table"), width = 16))
          )
  )
)

dashboardPage(header, sidebar, body)

# Define UI for application that draws a histogram

ui <- dashboardPage(header, sidebar, body)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #Create a subset to filter for County------------------------------------
  county_subset <- reactive({
    req(input$county)
    filter(crimes, crimes$County %in% input$county)
    })
  
  #Create a subset to filter by Year------------------------------------
  year_subset <- reactive({
    req(input$year)
    filter(crimes, crimes$Year %in% input$year)
  })
  
  # Data table ----------------------------------------------
  # output$table <- renderDataTable({
  #   DT::datatable(crimes,options = list(orderClasses = TRUE))
  #   })
  
  output$table <- renderDataTable({
    crimes
  })
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
