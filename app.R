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
    menuItem("Topline Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    menuItem("Dashboard", icon = icon("user-astronaut"), tabName = "tab"),
    menuItem("Crime Rates Drilldown", icon = icon("cog"), tabName = "crimerates"),
    
    # Inputs: county -----------------------------------------------------------------
    pickerInput("countySelect",
                label =  "Select Multiple Counties:",
                choices = sort(levels(crimes$County)),
                options = list(`actions-box` = TRUE),
                multiple = T, 
                selected = c("Erie", "Wyoming", "Orleans", "Bronx", "Hamilton", "Oneida")),
    
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
    
    h6("Change Upper Plot:"), 
    
    # Select Crime Type (RATE) ------------------------------------------------------------------
    radioButtons(inputId = "crime_type", 
                 label = "Select Type of Crime Rate: ",
                 choices = c("Firearms" = "Firearm.Rate",
                             "Violent Crimes" = "Violent.Rate",
                             "Index Crimes" = "Index.Rate",
                             "Property Crimes" = "Property.Rate"),
                 selected = "Firearm.Rate"), 
    
    h6("Change Lower Plot:"), 
    
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
          fluidPage(theme = shinytheme("yeti"),
            box(title = "Data Table: Filter by County, Population, and Year", titleWidth = 600, 
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
                   tabPanel("Index Rate", plotlyOutput(outputId = "plot1", height=650)),
                   tabPanel("Violent Rate", plotlyOutput(outputId = "plot2", height=650)),
                   tabPanel("Property Rate", plotlyOutput(outputId = "plot3", height=650)),
                   tabPanel("Firearms Rate", plotlyOutput(outputId = "plot4", height=650))
            )
          )
  ),
  
  # Dashboard---------------------
  
  # SOURCE: https://rstudio.github.io/shinydashboard/structure.html#boxes
  tabItem("tab",
          fluidRow(
            column(width = 6,
                   infoBoxOutput(outputId = "totalCrimes", 
                                 width = NULL),
                   infoBoxOutput(outputId = "avgFirearms",
                                 width = NULL),
                   infoBoxOutput(outputId = "avgViolent", 
                                 width = NULL),
                   infoBoxOutput(outputId = "avgProperty",
                                 width = NULL)),
            column(width = 6,
                   infoBoxOutput(outputId = "avgIndRate", 
                                 width = NULL),
                   infoBoxOutput(outputId = "avgFireRate", 
                                 width = NULL),
                   infoBoxOutput(outputId = "avgVilRate",
                                 width = NULL), 
                   infoBoxOutput(outputId = "avgPropRate", 
                                 width = NULL))
          ),
          fluidRow(
            box(title = "Information on Data Set",width =12, height = 200, status = "success",
                       "Date for this dashboard can be found on NY state's open data portal:
                https://data.ny.gov/Public-Safety/Index-Violent-Property-and-Firearm-Rates-By-County/34dd-6g2j
                
                Note: The New York City Police Department does not report the number of violent crimes 
                by firearm so those counts are not available for the five counties that comprise New York City. 
                
                Information on Key Terms: All rates are the number of crimes reported per 100,000 people and Index 
                refers to the total number of crimes, including: Murder, Rape, 
                Robbery, Aggravated Assault, Burglary, Larceny and Motor Vehicle Theft.")
          )
          
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
 
  ########################### DATA TABLE ################################ 
  
  # Data table ----------------------------------------------
  output$table <- DT::renderDataTable({

    
    DT::datatable(data = yearrange_CountySubset(), options = list(
      autoWidth = TRUE,
      scrollY = 450))
  })
  
  ########################### PLOTS ################################
  
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
      facet_wrap("Year") 
    
    ggplotly(p1)
  })
  
  # Crime Rate Drill Down-- Plot 2: Index Rate---------------------------
  
  output$plot2 <- renderPlotly({
    p2 <- ggplot(year_CountySubset(), aes_string(x = "County", y = "Violent.Rate", fill = "County")) +
      geom_bar(stat='identity', width=.5) +
      labs(subtitle="Rename'",
           title= paste("Violent Crime Rate in Selected Counties")) +
      coord_flip() +
      facet_wrap("Year") 
    
    ggplotly(p2)
    
  })
  
  #   # Crime Rate Drill Down-- Plot 3: Property Rate---------------------------
  
  output$plot3 <- renderPlotly({
    p3 <- ggplot(year_CountySubset(), aes_string(x = "County", y = "Property.Rate", fill = "County")) +
        geom_bar(stat='identity', width=.5) +
        labs(subtitle="Rename'",
             title= paste("Property Crime Rate in Selected Counties")) +
        coord_flip() +
        facet_wrap("Year") 
    
    ggplotly(p3)
  })
  
  # Crime Rate Drill Down-- Plot 4: Firearms Rate-------------------------
  output$plot4 <- renderPlotly({
    p4<- ggplot(year_CountySubset(), aes_string(x = "County", y = "Firearm.Rate", fill = "County")) +
      geom_bar(stat='identity', width=.5) +
      labs(subtitle="Rename'",
           title= paste("Firearm Crime Rate in Selected Counties")) +
      coord_flip() +
      facet_wrap("Year") 
    
    ggplotly(p4)
    
  })
  
  
  ########################### INFO BOXES #####################################################
  # as.numeric(mean(year_CountySubset()$Firearm.Rate)))
  
  # Total # of Crimes in Year info box ----------------------------------------------
  output$totalCrimes <- renderInfoBox({
    data <- year_CountySubset()
    sum <- sum(data$Index.Count)
    infoBox(paste(title = "Total # of Crimes in ", input$yearSelect),
            value = paste(sum, "Crimes"),
            icon = icon("cog"), color = "red")
  })
  
  # Avg # of Firearms Crimes in Year info box ----------------------------------------------
  output$avgFirearms <- renderInfoBox({
    data <- year_CountySubset()
    avg <- round(mean(data$Firearm.Count),0)
    infoBox(paste(title = "AVG # of Firearms Crimes in ", input$yearSelect),
            value = paste(avg, "Crimes"),
            icon = icon("cog"), color = "green")
  })
  
  # Avg # of Violent Crimes in Year info box ----------------------------------------------
  output$avgViolent <- renderInfoBox({
    data <- year_CountySubset()
    avg <- round(mean(data$Violent.Count),0)
    infoBox(paste(title = "AVG # of Violent Crimes in ", input$yearSelect),
            value = paste(avg, "Crimes"),
            icon = icon("cog"), color = "green")
  })
  
  # Avg # of Property Crimes in Year info box ----------------------------------------------
  output$avgProperty <- renderInfoBox({
    data <- year_CountySubset()
    avg <- round(mean(data$Property.Count),0)
    infoBox(paste(title = "AVG # of Property Crimes in ", input$yearSelect),
            value = paste(avg, "Crimes"),
            icon = icon("cog"), color = "green")
  })
  
  # Avg Rate of Firearms Crimes in Year info box ----------------------------------------------
  output$avgFireRate <- renderInfoBox({
    data <- year_CountySubset()
    avg <- round(mean(data$Firearm.Rate),0)
    infoBox(paste(title = "AVG Rate of Firearms Crimes in ", input$yearSelect),
            value = paste(avg, "Crimes"),
            icon = icon("bell"), color = "purple")
  })
  
  # Avg Rate of Violent Crimes in Year info box ----------------------------------------------
  output$avgVilRate <- renderInfoBox({
    data <- year_CountySubset()
    avg <- round(mean(data$Violent.Rate),0)
    infoBox(paste(title = "AVG Rate of Violent Crimes in ", input$yearSelect),
            value = paste(avg, "Crimes"),
            icon = icon("bell"), color = "purple")
  })
  
  # Avg Rate of Property Crimes in Year info box ----------------------------------------------
  output$avgPropRate <- renderInfoBox({
    data <- year_CountySubset()
    avg <- round(mean(data$Property.Rate),0)
    infoBox(paste(title = "AVG Rate of Property Crimes in ", input$yearSelect),
            value = paste(avg, "Crimes"),
            icon = icon("bell"), color = "purple")
  })

  # Avg Rate of ALL  Crimes in Year info box ----------------------------------------------
  output$avgIndRate <- renderInfoBox({
    data <- year_CountySubset()
    avg <- round(mean(data$Index.Rate),0)
    infoBox(paste(title = "AVG Rate of All Crimes in ", input$yearSelect),
            value = paste(avg, "Crimes"),
            icon = icon("cog"), color = "red")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

