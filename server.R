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
  
  output$type.pie <- renderPlotly(pies()[['type']] %>% layout(dragmode = "select") %>% config(displayModeBar = F))
  output$target.pie <- renderPlotly(pies()[['targets']] %>% layout(dragmode = "select"))
  output$weap.pie <- renderPlotly(pies()[['weap']] %>% layout(dragmode = "select"))
  
  output$click <- renderPrint({
    d <- event_data("plotly_click", source = 'select')
    print(d)
    if (is.null(d)) "Waiting for click" else d
  })
}
shinyServer(server)