library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
ui <- fluidPage(
  fluidRow(
    plotlyOutput("type.pie"),
    plotlyOutput("target.pie"),
    plotlyOutput("weap.pie")
  )
)
shinyUI(ui)