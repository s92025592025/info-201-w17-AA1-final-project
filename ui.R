library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
ui <- fluidPage(
  textInput('iso3', 'ISO3', value = "AFG"),
  fluidRow(
    plotlyOutput("type.pie"),
    plotlyOutput("target.pie"),
    plotlyOutput("weap.pie")
  ),
  verbatimTextOutput("click")
)
shinyUI(ui)