library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

source("analysis.R")

ui <- tagList(
  navbarPage( theme  =  shinythemes::shinytheme("united"), "Global Terrorism Database",
                  tabPanel("Summary"),
                  navbarMenu("DATA Exploration",
                             tabPanel("Data Vidualization",
                                      fluidPage(
                                        fluidRow(
                                          column(width =  4, textInput('iso3', 'ISO3', value = "AFG")),
                                          column(width = 4, sliderInput("year", "Year range:", min = 1970, max = 2015, value = c(1970,2015))),
                                          column(width = 4, selectInput("type", "Line Graph", 
                                                                        c("Multiple Attack" = "multiple",
                                                                        "Suicide Attack" = "suicide",
                                                                        "Sucessful Attack" = "success",
                                                                        selected = "success")))
                                        ),
                                        
                                        fluidRow(
                                          column(width = 4, selectInput("type.select", 'Select Attack Type', choices = c())),
                                          column(width = 4, selectInput("target.select",'Select Target Type', choices = c())),
                                          column(width = 4, selectInput("weap.select", 'Select Weapon Type',choices = c()))
                                        ),
                                        
                                        hr(),
                                        
                                        fluidRow(
                                          column(width = 8, plotlyOutput("graph")))
                                        ),
                                      
                                        hr(),
                                      
                                        fluidRow(
                                          column(width = 4, plotlyOutput("type.pie")),
                                          column(width = 4, plotlyOutput("target.pie")),
                                          column(width = 4, plotlyOutput("weap.pie"))
                                        ),
                                        verbatimTextOutput("click"),
                                      hr(),
                                          div(plotlyOutput("plot")) 
                                      ),
                             tabPanel("Data Table"))
))

shinyUI(ui)