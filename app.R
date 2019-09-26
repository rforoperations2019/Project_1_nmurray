library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(tools)
library(stringr)

# Load Data---------
crimes <- read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv")
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
              choices = str_to_title(c("ALBANY","ALLEGANY","BRONX","BROOME","CATTARAUGUS", "CAYUGA",
                          "CHAUTAUQUA", "CHEMUNG", "CHENANGO", "CLINTON", "COLUMBIA","CORTLAND",
                          "DELAWARE", "DUTCHESS", "ERIE", "ESSEX", "FRANKLIN", "FULTON",
                          "GENESEE", "GREENE", "HERKIMER", "JEFFERSON", "KINGS",  "LEWIS",
                          "LIVINGSTON", "MADISON", "MONROE", "MONTGOMERY", "NASSAU", "NEW YORK",
                          "NIAGARA", "ONEIDA", "ONONDAGA", "ONTARIO", "ORANGE", "ORLEANS",
                          "OSWEGO", "OTSEGO", "PUTNAM", "QUEENS", "RENSSELAER", "RICHMOND",
                          "ROCKLAND", "SARATOGA", "SCHENECTADY", "SCHOHARIE", "SCHUYLER", "SENECA",
                          "ST LAWRENCE", "STEUBEN", "SUFFOLK", "SULLIVAN","TIOGA","TOMPKINS",
                          "ULSTER", "WARREN", "WASHINGTON", "WAYNE", "WESTCHESTER", "WYOMING"))),
  
  # Select Date Range for Filter by year (Over a Interval)---------------------------

  sliderInput(inputId = "year_range", label = "Year Range:", 
              min = 1990, max = 2018, value = c(1990, 2018), ticks = TRUE, sep = "", 
              animate = animationOptions(interval = 500)) # Animates over time--Not sure I want this? 
  
  )
)

body <- dashboardBody(
  # Data Table Page 
          fluidPage(
            box(DT::dataTableOutput(outputId = "table"))
          ),
  
  # Plot Page

  tabItems(
    #Firearms Crimes Plot------------------------------------------------------
    tabItem(
      fluidPage(
        box(title = "Plot--Not sure", width = 12,
            plotOutput(outputId = "plot_firearmrt"))

      )
    )
  )
)
  
dashboardPage(header, sidebar, body)

# Define UI for application that draws a histogram

ui <- dashboardPage(header, sidebar, body)

############## SERVER ###########################
server <- function(input, output) {
  
  #Create a subset to filter by Year And County------------------------------------
  year_CountySubset <- reactive({
      req(input$year, input$county)
      subset(crimes$Year >= input$year[1] &
             crimes$Year <= input$year[2] &
            crimes$County == input$county)
  })

 # Data table ----------------------------------------------
 output$table <- renderDataTable({
     DT::datatable(year_CountySubset(),options = list(orderClasses = TRUE))
   })
 
 # Density Plot of Year ----------------------------
 output$plot_firearmrt <- renderPlot({
   ggplot(year_CountySubset(), aes(x =Year, fill = Firearm.Rate)) +
     geom_density() + labs(title="Firearms Crimes Rate by County", x="Year")
 }
 )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

