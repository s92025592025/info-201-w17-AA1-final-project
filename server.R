library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(stringr)
library(plotly)
library(scales)


source("analysis.R")

server <- function(input, output) {

# Passes the input recieved from the ui to a function to get the plot.
  output$graph <- renderPlotly({
    return(Global.Terrorism.Attacks(input$slider))
  })
}

shinyServer(server)
