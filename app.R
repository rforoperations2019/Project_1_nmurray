library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(tools)
library(stringr)
library(shinythemes)
library(shinyWidgets)
library(ggalt)

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
  
  
  # Select Crime Type (RATE) ------------------------------------------------------------------
  radioButtons(inputId = "crime_type", 
               label = "Select Type of Crime Rate: ",
               choices = c("Firearms" = "Firearm.Rate",
                           "Violent Crimes" = "Violent.Rate",
                           "Index Crimes" = "Index.Rate",
                           "Property Crimes" = "Property.Rate"),
               selected = "Firearm.Rate"), 
  
  # Select Crime Type (Count) ------------------------------------------------------------------
  radioButtons(inputId = "crime_count", 
               label = "Select Type of Crime Count: ",
               choices = c("Firearms" = "Firearm.Count",
                           "Violent Crimes" = "Violent.Count",
                           "Index Crimes" = "Index.Count",
                           "Property Crimes" = "Property.Count"),
               selected = "Firearm.Count") 
  
  )
)

body <- dashboardBody(tabItems(
  # Data Table Page -----------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Data Table: Filter by County, Population, and Year", titleWidth = 450, 
                DT::dataTableOutput(outputId = "table"))
          )
  ),
  
  # Plot Page ------------------------------------------
  tabItem("plot",
        fluidPage(
          box(title = "Selected Crime Rate in Selected NYS Counties", width = 10,
            plotlyOutput(outputId = "plot_firearmrt"))
      ),
      fluidPage(
        box(title = "Raw Crime Count By Population", width = 10,
            plotlyOutput(outputId = "scatter_crimepop"))
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
                             crimes$County %in% input$countySelect &
                              (crimes['Population'] >= input$pop_range[1]) & (crimes['Population'] <= input$pop_range[2]),]
    data_subset

  })
  # Subset to Filter by County and Population
  county_popSubset <-reactive({
    req(input$countySelect,input$pop_range)
    data_subset <- crimes[crimes$County %in% input$countySelect &
                            (crimes['Population'] >= input$pop_range[1]) & (crimes['Population'] <= input$pop_range[2]),]
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
  
  # Subset to Filter by Crime Type  ---------------------------------------
  firearmsSubset <- reactive({
      req(input$crime_type)
      data_subset <-filter(crimes, title_type %in% input$selected_type)
  })
  

 # Data table ----------------------------------------------
 output$table <- DT::renderDataTable({
     year_CountySubset()
   })
 
 
 # Bar PLot ----------------------------  Crime  Rate Type by County
 output$plot_firearmrt <- renderPlotly({
      g <-ggplot(year_CountySubset(), aes_string(x ="Year", y = input$crime_type, fill = "County")) +
      geom_bar(stat = 'identity') +
      labs(title=paste( input$crime_type, "Crimes in", input$countySelect, sep = " "), x="Year")

      ggplotly(g)
 })
 
 # Scatter Plot------------------------ Raw Crime Count By Population
 
 output$scatter_crimepop <- renderPlotly({
   p <- ggplot(year_CountySubset(), aes_string(x = "Population", y = input$crime_count, fill ="County")) + 
     geom_point() + labs(title=paste( input$crime_count, "Crimes in", input$countySelect, sep = " "), x="Population", y = "input$crime_count") +
     theme(axis.text.x = element_text(angle = 45)) +
     facet_wrap("Year")
     
  
   ggplotly(p)


 })
 
 
  

}

# Run the application 
shinyApp(ui = ui, server = server)

