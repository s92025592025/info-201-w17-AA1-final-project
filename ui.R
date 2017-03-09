library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

source("analysis.R")

ui <- tagList(
  navbarPage( theme  =  shinythemes::shinytheme("united"), "Global Terrorism Database",
                  tabPanel("Introduction"),
                             tabPanel("Data Visualization",
                                      fluidPage(
                                        fluidRow(
                                          column(width =  4, textInput('iso3', 'ISO3', value = "AFG")),
                                          column(width = 4, sliderInput("year", "Year range:", min = 1970, max = 2015, value = c(1970,2015))),
                                          column(width = 8, plotlyOutput("graph"))
                                        ),
                                        
                                        tags$hr(style="border-color: black;"),
                                        
                                        fluidRow(
                                          column(width = 4, selectInput("type.select", 'Select Attack', choices = c())),
                                          column(width = 4, selectInput("target.select",'Select Target Type', choices = c())),
                                          column(width = 4, selectInput("weap.select", 'Select Weapon Type',choices = c())),
                                          column(width = 4, selectInput("type", "Select Attack Type", 
                                                                        c("Multiple Attack" = "multiple",
                                                                          "Suicide Attack" = "suicide",
                                                                          "Sucessful Attack" = "success",
                                                                          selected = "success"))
                                        )),
                                        
                                        tags$hr(style="border-color: black;"),

                                        fluidRow(
                                          column(width = 4, selectInput("type.select", 'Select Attack Type', choices = c()),
                                                            plotlyOutput("type.pie")),
                                          column(width = 4, selectInput("target.select",'Select Target Type', choices = c()),
                                                            plotlyOutput("target.pie")),
                                          column(width = 4, selectInput("weap.select", 'Select Weapon Type',choices = c()),
                                                            plotlyOutput("weap.pie"))
                                        ),
                                        verbatimTextOutput("click"),
                                        tags$hr(style="border-color: black;"),
                                          div(plotlyOutput("plot")) 
                                      )
)))

shinyUI(ui)