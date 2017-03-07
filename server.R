library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(stringr)
library(plotly)
library(scales)


server <- function(input, output) {
  

  output$graph <- renderLeaflet({
    return(Global.Terrorism.Attacks(input$slider))
  })
}

shinyServer(server)
