library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
x <- c(1,2,3,4,5,6)
y <- c(1,2,3,4,5,6)
test <- data.frame(x,y)
plot <- plot_ly(test, x=x, y=y)
server <- function(input, output, clientData, session) {
  
  # This is a reactive varaible that finds a new set of pie info when input parameters are changed
  pies <- reactive({
    d <- event_data("plotly_click")
    Attack.Info.Pies(input$iso3, c(2015,2015), c())
  })
  
  lists <- reactive({
    Attack.Info.List(input$iso3, c(2015,2015), c()) 
  })
 
  # The three pie charts to disply attack information
  output$type.pie <- renderPlotly(pies()[['type']])
  output$target.pie <- renderPlotly(pies()[['targets']])
  output$weap.pie <- renderPlotly(pies()[['weap']])
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    print(d)
    if (is.null(d)) "Waiting for click" else d
  })
  
  observe({ # Listen to when the to-be-included attributes are changed
    lists <- lists()
    updateSelectInput(session, 'type.select', choices = lists[['type']]) # Change the attribute choices for plot's x-axis
    updateSelectInput(session, 'info.select', choices = lists[['target']]) # Change the attribute choices for plot's y-axis
    updateSelectInput(session, 'weap.select', choices = lists[['weap']]) # Change the attribute choices for table's sorting method
  })
  
}
shinyServer(server)