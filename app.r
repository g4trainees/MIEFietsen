
#################################################################################################
## R shiny app voor fietsenstallingen
#
# Poolopdracht G4 trainees informatiemanagement sep 2018- feb 2019
# Scrumteam Mobile Informatie Eenheid
#
# Auteurs: Jurriaan Duyne (jurriaan.duyne@utrecht.nl), Alma Raskadic en Pieter Goethart, Nizar el Haik en Dominique de Graaf.
#
# Bij dit dashboard hoort "Inlees en verwerkingsscript.R" waarin ruwe data van FMS en Lumiguide verwerkt wordt. Deze data wordt
# ingeladen voor deze app.

################################################################
## Packages laden (moeten eventueel nog geinstalleerd worden install.packages('PACKAGE_NAME'))
library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(leaflet)
library(shinydashboard)
library(xtable)

## Afspraak, zet je working directory op de map waar de app.R file in staat
# setwd("~/G4 Traineeship/Poolopdrachten/3 - Strategische opgaven (Utrecht)/Mobiele Informatie Eenheid/Dashboards/Dashboard R")

### Data laden die is aangemaakt door het Inlees- en verwerkingsscript

#plaats waar de data is opgeslagen
outputDirectory <- paste0(getwd(),"/Data/Output")

# data frames laden
bezetting <- readRDS(file = paste0(outputDirectory,"/", "bezetting.RDS"))
stallingsduur <- readRDS(file = paste0(outputDirectory,"/", "stallingsduur.RDS"))
checkInOutPerUur <- readRDS(file = paste0(outputDirectory,"/", "checkInOutPerUur.RDS"))
coordinaten <- readRDS(file = paste0(outputDirectory,"/", "coordinaten.RDS"))
  

###############################################
# R shiny specifieke code
header <- dashboardHeader(
  title = "Fietsparkeerinzichten"
)

# UI maken met elementen die worden geladen
body <- dashboardBody(
  fluidRow(
    column(3,
           titlePanel("Selecteer de gewenste data"),
           dateInput('dateBegin',
                     label = 'Startdatum: yyyy-mm-dd',
                     value = '2018-06-01'),
           dateInput('dateEinde',
                     label = 'Einddatum: yyyy-mm-dd',
                     value = '2018-06-09')
    ),
    #br(),# br() maakt wat whitespace
    column(4,
           #titlePanel("Kies locatie"),
           leafletOutput("mymap")
    ),
    #br(),
    column(5,
           plotlyOutput("stallingsDuur")      
    )
  ),
  br(),
  fluidRow(
    column(6,
           plotlyOutput("plotInOut")
    ),
    column(6,
           plotlyOutput("plotBezetting")
    )
  )
)  

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

# "serverside" code waarmee de plaatjes gemaakt worden
server <- function(input, output) {
  
  # Een kaart om de locatie op te selecteren, nu alleen nog een stub
  output$mymap <- renderLeaflet({
    m <- leaflet(data = coordinaten) %>%
      addTiles() %>%
      setView(lng = 5.114168, lat = 52.09266, zoom = 13.5) %>%
      addMarkers(lng = ~long, lat = ~lat, popup=~as.character(Locatie), layerId = ~as.character(Locatie)) 
    m
  })
  
  #Elke keer dat de input variabelen (locatie en data) veranderen moet de data weer opnieuwe worden gesliced
  stallingsduur_subset <- reactive({
    if(is.null(input$mymap_marker_click$id)) {
      tempLocatie <- "Stadhuis"
    } else {
      tempLocatie <- input$mymap_marker_click$id 
    }
    a <- subset(stallingsduur,  Tijd >= input$dateBegin & Tijd <= input$dateEinde & Locatie == tempLocatie)
    a <- table(a$duratie, a$Weekdag)
    a <- data.frame(rbind(t(a)))
    colnames(a) <- c("korter dan 1 uur","tussen 1 en 4 uur","tussen 4 en 12 uur","tussen 12 en 24 uur","langer dan 24 uur")
    return(a)
  })
  
  # Georderde dagen voor plotly
  xform <- list(categoryorder = "array",
                categoryarray = c("maandag", "dinsdag", "woensdag", "donderdag", 
                                  "vrijdag", "zaterdag", "zondag"))
 
  # Output stacked bar chart plotly voor bezettingsduur
  output$stallingsDuur <- renderPlotly({
    plot_ly(stallingsduur_subset(), x = xform$categoryarray, y = ~`korter dan 1 uur`, type = 'bar', name = 'korter dan een uur') %>%
      add_trace(y = ~`tussen 1 en 4 uur`, name = 'tussen 1 en 4 uur') %>%
      add_trace(y = ~`tussen 4 en 12 uur`, name = 'tussen 4 en 12 uur') %>%
      add_trace(y = ~`tussen 12 en 24 uur`, name = 'tussen tussen 12 en 24 uur') %>%
      add_trace(y = ~`langer dan 24 uur`, name = 'langer dan 24 uur') %>%
      layout(xaxis = xform, yaxis = list(title = 'Aantallen'), barmode = 'stack')
  })
  
  # Plotly plot voor check ins en outs per uur
  output$plotInOut <- renderPlotly({
    if(is.null(input$mymap_marker_click$id)) {
      tempLocatie <- "Stadhuis"
    } else {
      tempLocatie <- input$mymap_marker_click$id 
    }
    plot_ly(subset(checkInOutPerUur, Tijd >= input$dateBegin & Tijd <= input$dateEinde & Locatie == tempLocatie), x = ~Tijd, y = ~`Aantal checkins`, name = 'Aantal Checkins', type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(205, 12, 24)', width = 2)) %>%
      add_trace(y = ~`Aantal checkouts`, name = 'Aantal checkouts', line = list(color = 'rgb(22, 96, 167)', width = 2)) %>%
      layout(legend = list(x = 0.1, y = 0.9)) %>%
      layout(title = "Aantal fietsen checkins en checkouts",
             xaxis = list(title = "Uren"),
             yaxis = list (title = "Aantallen"))
  })
  
  # Plotly plot voor Bezettingsgraad per uur
  output$plotBezetting <- renderPlotly({
    if(is.null(input$mymap_marker_click$id)) {
      tempLocatie <- "Stadhuis"
    } else {
      tempLocatie <- input$mymap_marker_click$id 
    }
    plot_ly(subset(bezetting, Tijd >= input$dateBegin & Tijd <= input$dateEinde & Locatie == tempLocatie), x = ~Tijd, y = ~BezettingspercentageUur, name = "Gemiddelde bezetting per uur", type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(205, 12, 24)', width = 2)) %>%
      layout(title = "Bezettingsgraad per uur",
             xaxis = list(title = "Uren"),
             yaxis = list (title = "% gemiddelde bezetting per uur"))
  })
  
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

