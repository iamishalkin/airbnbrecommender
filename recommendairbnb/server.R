#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
sea <- read.csv("~/SeaTravel/SeaTravel/seasearcheng2.csv")
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  output$x1 <- DT::renderDataTable({ datatable(aparts, selection = 'none', class = 'cell-border strip hover'
  ) %>% formatStyle(0, cursor = 'pointer')
  })
  
  observeEvent(input$x1_cell_clicked, {
    info = input$x1_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 1st column
    if (is.null(info$value) || info$col != 0) return()
    updateTabsetPanel(session, 'x0', selected = 'Plot')
    updateTextInput(session, 'x2', value = info$value)
  })
  output$x3 = DT::renderDataTable(iris)
  
}