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
    menuItem("Crimes Rates Drilldown", icon = icon("cog"), tabName = "crimerates"),
    
    # Inputs: county -----------------------------------------------------------------
    pickerInput("countySelect",
                label =  "Select County:",
                choices = sort(levels(crimes$County)),
                options = list(`actions-box` = TRUE),
                multiple = T, 
                selected = "Erie"),
    
    # # Select Date Range for Filter by year (Over a Interval)---------------------------
    # sliderInput(inputId = "year_range", label = "Select Year Range:", 
    #             min = 1990, max = 2018, value = c(1990, 2018), ticks = TRUE, sep = "", 
    #             animate = animationOptions(interval = 500)), # Animates over time--Not sure I want this? 
    
    # SelectYEAR---------------------------
    selectInput("yearSelect",
                label =  "Select Year:",
                choices = c(seq(1990,2018, by = 1)),
                selected = "1990"),
    
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
            box(title = "Crime Rate in Selected NYS Counties over Time", width = 10,
                plotlyOutput(outputId = "plot_firearmrt"))
          ),
          fluidPage(
            box(title = "Crime Count in Selected NYS Counties By Year ", width = 10,
                plotlyOutput(outputId = "scatter_crimepop"))
          )
  ),
  
  # Crime Rate DrillDown Page ----------------------------------------------
  tabItem("crimerates",
          fluidRow(
            tabBox(title = "Crime Rates",
                   width = 12,
                   tabPanel("Index Rate", plotlyOutput(outputId = "plot1")),
                   tabPanel("Violent Rate", plotlyOutput(outputId = "plot2")),
                   tabPanel("Property Rate", plotlyOutput(outputId = "plot3")),
                   tabPanel("Property Rate", plotlyOutput(outputId = "plot4"))
            )
          )
  ),
  
  # Dashboard---------------------
  tabItem("Third tab", 
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
  
  # #Create a subset to filter by And County and Population------------------------------------
  yearrange_CountySubset <- reactive({
    req(input$countySelect, input$pop_range)
    data_subset <- crimes[crimes$County %in% input$countySelect &
                            (crimes['Population'] >= input$pop_range[1]) & (crimes['Population'] <= input$pop_range[2]),]
    data_subset
    
  })
  
  # #Create a subset to filter by SINGLE Year And County and Population------------------------------------
  year_CountySubset <- reactive({
    req(input$yearSelect, input$countySelect, input$pop_range)
    data_subset <- crimes[crimes$Year %in% input$yearSelect &
                            crimes$County %in% input$countySelect &
                            (crimes['Population'] >= input$pop_range[1]) & (crimes['Population'] <= input$pop_range[2]),]
    data_subset
    
  })
  
  
  # Data table ----------------------------------------------
  output$table <- DT::renderDataTable({
    yearrange_CountySubset()
  })
  
  
  # Bar PLot ----------------------------  Crime  Rate Type by County
  output$plot_firearmrt <- renderPlotly({
    g <-ggplot(yearrange_CountySubset(), aes_string(x ="Year", y = input$crime_type, fill = "County")) +
      geom_bar(stat = 'identity') +
      labs(title=paste(input$crime_type, "Crimes Rates", sep = " "), x="Year")
    
    ggplotly(g)
  })
  
  # Scatter Plot------------------------ Raw Crime Count By Population
  
  output$scatter_crimepop <- renderPlotly({
    p <- ggplot(year_CountySubset(), aes_string(x = "Population", y = input$crime_count, fill ="County")) + 
      geom_point() + labs(title=paste(input$crime_count, "Crimes Counts", sep = " "), x="Population", y = "input$crime_count") +
      theme(axis.text.x = element_text(angle = 45)) +
      facet_wrap("Year")
    
    
    ggplotly(p)
    
    
  })
  
  # Crime Rate Drill Down-- Plot 1: Index Rate---------------------------
  
  output$plot1 <- renderPlotly({
    p1 <- ggplot(year_CountySubset(), aes_string(x = "County", y = "Index.Rate", fill = "County")) +
      geom_bar(stat='identity', width=.5) +
      labs(subtitle="Rename'",
           title= paste("Index Crime Rate in Selected Counties")) +
      coord_flip() +
      facet_wrap("Year") +
      geom_vline(xintercept = as.numeric(mean(year_CountySubset()$Index.Rate)))
    
    ggplotly(p1)
  })
  
  # Crime Rate Drill Down-- Plot 2: Index Rate---------------------------
  
  output$plot2 <- renderPlotly({
    p2 <- ggplot(year_CountySubset(), aes_string(x = "County", y = "Violent.Rate", fill = "County")) +
      geom_bar(stat='identity', width=.5) +
      labs(subtitle="Rename'",
           title= paste("Violent Crime Rate in Selected Counties")) +
      coord_flip() +
      facet_wrap("Year") +
      geom_vline(xintercept = as.numeric(mean(year_CountySubset()$Violent.Rate)))
    
    ggplotly(p2)
    
  })
  
  #   # Crime Rate Drill Down-- Plot 3: Property Rate---------------------------
  
  output$plot3 <- renderPlotly({
    p3 <- ggplot(year_CountySubset(), aes_string(x = "County", y = "Property.Rate", fill = "County")) +
        geom_bar(stat='identity', width=.5) +
        labs(subtitle="Rename'",
             title= paste("Property Crime Rate in Selected Counties")) +
        coord_flip() +
        facet_wrap("Year") +
        geom_vline(xintercept = as.numeric(mean(year_CountySubset()$Property.Rate)))
    
    ggplotly(p3)
  })
  
  # Crime Rate Drill Down-- Plot 4: Firearms Rate-------------------------
  output$plot4 <- renderPlotly({
    p4<- ggplot(year_CountySubset(), aes_string(x = "County", y = "Firearm.Rate", fill = "County")) +
      geom_bar(stat='identity', width=.5) +
      labs(subtitle="Rename'",
           title= paste("Firearm Crime Rate in Selected Counties")) +
      coord_flip() +
      facet_wrap("Year") +
      geom_vline(xintercept = as.numeric(mean(year_CountySubset()$Firearm.Rate)))
    
    ggplotly(p4)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

