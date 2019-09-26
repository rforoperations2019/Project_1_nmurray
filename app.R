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

header <- dashboardHeader(title = "New York State Crime Data 1990-2018", 
                          titleWidth = 450)

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
  sliderInput(inputId = "pop_range", label = "Select Counties with Population Size of:", 
              min = 0, max = max(crimes$Population), value = c(min(crimes$Population),max(crimes$Population)), ticks = TRUE), 
  
  
  # Select Crime Type ------------------------------------------------------------------
  radioButtons(inputId = "crime_type", 
               label = "Select Type of Crime Rate: ",
               choices = c("Firearms" = "Firearm.Rate",
                           "Violent Crimes" = "Violent.Rate",
                           "Index Crimes" = "Index.Rate",
                           "Property Crimes" = "Property.Rate"),
               selected = "Firearm.Rate")
  
  )
)

body <- dashboardBody(tabItems(
  # Data Table Page -----------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Title of Data table", DT::dataTableOutput(outputId = "table"))
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

ui <- dashboardPage(skin = "green", header, sidebar, body)

############## SERVER ###########################

server <- function(input, output) {
  
  # #Create a subset to filter by Year And County and Population------------------------------------
  year_CountySubset <- reactive({
      req(input$year_range, input$countySelect, input$pop_range)
      data_subset <- crimes[(crimes['Year'] >= input$year_range[1]) & (crimes['Year'] <= input$year_range[2]) &
                             crimes$County %in% input$countySelect,]
    data_subset

  })
  
  # Subset to Filter by County ONLY ---------------------------------------------------
  CountySubset <- reactive({
      req(input$countySelect)
      data_subset <- crimes[crimes$County %in% input$countySelect,]
      data_subset
  })
  
  # Subset to Filter by Population Size ONLY-----------------------------------------------
  popSubset <- reactive({
    req(input$pop_range)
    data_subset <- crimes[crimes$Population %in% input$pop_range,]
    data_subset
  })
  
  # Subset to Filter by Crime Type  ---------------------------------------NOT CURRENTLY REACTIVE 
  firearmsSubset <- reactive({
      req(input$crime_type)
      data_subset <-filter(crimes, title_type %in% input$selected_type)
  })
  

 # Data table ----------------------------------------------
 output$table <- DT::renderDataTable({
     year_CountySubset()
   })
 
 
 # Density Plot of Year by County ----------------------------  CHANGE TO PLOTLY, PLOT NAME CURRENT JUST 1
 output$plot_firearmrt <- renderPlot({
   print(input$crime_type)
      ggplot(CountySubset(), aes_string(x ="Year", y = input$crime_type )) +
      geom_bar(stat = 'identity') + labs(title=paste( input$crime_type, "Crimes in", input$countySelect, sep = " "), x="Year")   ### Insert reactive 
 }
 )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

