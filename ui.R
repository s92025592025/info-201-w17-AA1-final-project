library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("analysis.R")
ui <- fluidPage(
  textInput('iso3', 'ISO3', value = "AFG"),
  fluidRow(
    column(width = 4, selectInput("type.select", 'Select Attack Type', choices = c(''))),
    column(width = 4, selectInput("target.select",'Select Target Type', choices = c(''))),
    column(width = 4, selectInput("weap.select", 'Select Weapon Type',choices = c('')))
  ),
  fluidRow(
    column(width = 4, plotlyOutput("type.pie")),
    column(width = 4, plotlyOutput("target.pie")),
    column(width = 4, plotlyOutput("weap.pie"))
  ),
  verbatimTextOutput("click")
)
shinyUI(ui)