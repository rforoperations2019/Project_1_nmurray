library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(tools)
library(stringr)
library(shinythemes)

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
    menuItem("RENAME THIS", icon("plot"), tabName = "Third tab"),
  
  
  # Inputs: select variables to plot ----------------------------------------------
  selectInput("countySelect",
              label =  "Select County:",
              choices = sort(unique(crimes$County)),
              multiple = TRUE,
              selectize = TRUE,
              selected = c("Erie", "Wyoming")),
  
  # Select Date Range for Filter by year (Over a Interval)---------------------------
  sliderInput(inputId = "year_range", label = "Year Range:", 
              min = 1990, max = 2018, value = c(1990, 2018), ticks = TRUE, sep = "", 
              animate = animationOptions(interval = 500)) # Animates over time--Not sure I want this? 
  
  )
)

body <- dashboardBody(tabItems(
  # Data Table Page -----------------------------------
  tabItem("table",
          fluidPage(
            box(DT::dataTableOutput(outputId = "table"))
          )
  ),
  
  # Plot Page ------------------------------------------
  tabItem("plot",
      fluidPage(
        box(title = "Plot--Not sure", width = 12,
            plotOutput(outputId = "plot_firearmrt"))
      )
  ),
  
  # Third Tab--Not Sure What goes here-------------------
  tabItem("Third tab", 
          fluidPage())
  )
)
  
############## UI #############################

ui <- dashboardPage(header, sidebar, body)

############## SERVER ###########################

server <- function(input, output) {
  
  # #Create a subset to filter by Year And County------------------------------------
  year_CountySubset <- reactive({
      req(input$year_range, input$countySelect)
     # data_subset <- crimes
     data_subset <- crimes[(crimes['Year'] >= input$year_range[1]) & (crimes['Year'] <= input$year_range[2]) &
                             crimes$County %in% input$countySelect,]
     data_subset

  })

 # Data table ----------------------------------------------
 output$table <- DT::renderDataTable({
     year_CountySubset()
   })
 
 
 # Density Plot of Year ----------------------------
 output$plot_firearmrt <- renderPlot({
   ggplot(year_CountySubset(), aes(x =Year, fill = Firearm.Rate)) +
     geom_density() + labs(title=paste("Firearms Crimes Rate in", input$countySelect, sep = " "), x="Year")   ### Insert reactive 
 }
 )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

