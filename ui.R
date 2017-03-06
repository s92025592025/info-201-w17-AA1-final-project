library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
ui <- fluidPage(
  textInput('iso3', 'ISO3', value = "AFG"),
  fluidRow(
    column(width = 4, plotlyOutput("type.pie")),
    column(width = 4, plotlyOutput("target.pie")),
    column(width = 4, plotlyOutput("weap.pie"))
  ),
  verbatimTextOutput("click")
)
shinyUI(ui)