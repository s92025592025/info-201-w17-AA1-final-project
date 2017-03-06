library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
ui <- fluidPage(
  textInput('iso3', 'ISO3', value = "AFG"),
  fluidRow(
    plotlyOutput("type.pie", click = "type_click"),
    plotlyOutput("target.pie", click = "target_click"),
    plotlyOutput("weap.pie", click = "weap_click")
  )
)
shinyUI(ui)