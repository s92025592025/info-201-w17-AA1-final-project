library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(stringr)
library(plotly)
library(scales)


source("analysis.R")


server <- function(input, output, clientData, session) {
  
  # This is a reactive varaible that finds a new set of pie info when input parameters are changed
  pies <- reactive({
    select <- select()
    year <- year()
    print(year)
    Attack.Info.Pies(input$iso3, year, select)
  })
  
  select <- reactive({
    type <- input$type.select
    target <- input$target.select
    weap <- input$weap.select
    select = list()
    if(type != 'ALL') select <- c(select, 'attacktype' = type)
    if(target != 'ALL') select <- c(select, 'targtype' = target)
    if(weap != 'ALL') select <- c(select, 'weaptype' = weap)
    select
  })
  
  year <- reactive({
    year <- input$year
    return(c(year))
    year
  })
  
  lists <- reactive({
    select <- select()
    year <- year()
    Attack.Info.List(input$iso3, year, select)
  })
 
  # Passes the input recieved from the ui to a function to get the plot.
  output$graph <- renderPlotly({
      return(Global.Terrorism.Attacks(year()[1],year()[2]))
      
  })
  
  
  # The three pie charts to disply attack information
  output$type.pie <- renderPlotly({pies()[['type']]})
  output$target.pie <- renderPlotly(pies()[['targets']])
  output$weap.pie <- renderPlotly(pies()[['weap']])
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    print(d)
    if (is.null(d)) "Waiting for click" else d
  })
  
  observe({ # Listen to when the to-be-included attributes are changed
    lists <- Attack.Info.List(input$iso3, c(2015,2015), c())
    updateSelectInput(session, 'type.select', choices = lists[['type']]) # Change the attribute choices for plot's x-axis
    updateSelectInput(session, 'target.select', choices = lists[['target']]) # Change the attribute choices for plot's y-axis
    updateSelectInput(session, 'weap.select', choices = lists[['weap']]) # Change the attribute choices for table's sorting method
  })
  
  output$plot.multiple <- renderPlotly({
    return(compare.rates("Multiple", input$type.select))
  })
  
  output$plot.success <- renderPlotly({
    return(compare.rates("Success", input$type.select))
  })
  
  output$plot.suicide <- renderPlotly({
    return(compare.rates("Suicide", input$type.select))
  })
  
}
shinyServer(server)

