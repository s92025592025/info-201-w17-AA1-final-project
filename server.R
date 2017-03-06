library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
server <- function(input, output) {
  pies <- reactive({
    d <- event_data("plotly_click")
    Attack.Info.Pies(input$iso3, c(2015,2015), c())
  })
  
  output$type.pie <- renderPlotly(pies()[['type']])
  output$target.pie <- renderPlotly(pies()[['targets']])
  output$weap.pie <- renderPlotly(pies()[['weap']])
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    print(d)
    if (is.null(d)) "Waiting for click" else d
  })
}
shinyServer(server)