library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(tools)
library(stringr)
library(shinythemes)
library(shinyWidgets)

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
    menuItem("Dashboard", icon = icon("user-astronaut"), tabName = "Third tab"),
  

  # Inputs: county -----------------------------------------------------------------
  pickerInput("countySelect",
              label =  "Select County:",
              choices = sort(levels(crimes$County)),
              options = list(`actions-box` = TRUE),
              multiple = T, 
              selected = "Erie"),
  
  # Select Date Range for Filter by year (Over a Interval)---------------------------
  sliderInput(inputId = "year_range", label = "Select Year Range:", 
              min = 1990, max = 2018, value = c(1990, 2018), ticks = TRUE, sep = "", 
              animate = animationOptions(interval = 500)), # Animates over time--Not sure I want this? 
  
  # Select Population Range-------------------------------------------------------------
  
  # Select Crime Type ------------------------------------------------------------------
  radioButtons(inputId = "crime_type", 
               label = "Select Crime Type: ",
               choices = c("Firearms" = "Firearm.Rate",
                           "Violent Crimes" = "Violent.Rate",
                           "Index Crimes" = "Index.Rate",
                           "Property Crimes" = "Property.Rate"),
               selected = "FIrearms")
  # Select Region -----------------------------------------------------------------------
  
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
  
  # Dashboard---------------------
  tabItem("Dashboard", 
          fluidPage(infoBox(title = "This is a title", value = NULL, subtitle = NULL,
                            icon = shiny::icon("bar-chart"), color = "aqua", width = 4,
                            href = NULL, fill = FALSE))
  )
)
)
############## UI #############################

ui <- dashboardPage(header, sidebar, body)

############## SERVER ###########################

server <- function(input, output) {
  
  # #Create a subset to filter by Year And County------------------------------------
  year_CountySubset <- reactive({
    req(input$year_range, input$countySelect)
    data_subset <- crimes[(crimes['Year'] >= input$year_range[1]) & (crimes['Year'] <= input$year_range[2]) &
                             crimes$County %in% input$countySelect,]
    data_subset

  })
  
  # Subset to Filter by County ONLY 
  CountySubset <- reactive({
    req(input$countySelect)
    data_subset <- crimes[crimes$County %in% input$countySelect,]
    data_subset
  })
  
  # Subset to Filter by Crime Type  ---------------------------------------NOT CURRENTLY REACTIVE 
  CrimeSubset <- reactive({
    req(input$crime_type)
    data_subset <-filter(crimes, title_type %in% input$selected_type)
    })
  

 # Data table ----------------------------------------------
 output$table <- DT::renderDataTable({
     year_CountySubset()
   })
 
 
 # Density Plot of Year by County ----------------------------  CHANGE TO PLOTLY
 output$plot_firearmrt <- renderPlot({
   ggplot(CountySubset(), aes(x =Year, fill = Firearm.Count)) +
     geom_density() + labs(title=paste("Firearms Crimes Count in", input$countySelect, sep = " "), x="Year")   ### Insert reactive 
 }
 )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

