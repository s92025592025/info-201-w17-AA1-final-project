library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
server <- function(input, output, clientData, session) {
  pies <- Attack.Info.Pies('AFG', c(2015,2015), c())
  type.pie <- pies$type
  target.pie <- pies$targets
  weap.pie <- pies$weap
  output$type.pie <- renderPlotly({type.pie})
  output$target.pie <- renderPlotly({target.pie})
  output$weap.pie <- renderPlotly({target.pie})
}
shinyServer(server)