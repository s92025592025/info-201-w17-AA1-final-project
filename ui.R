# Loads the required library
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Loads the file
source("analysis.R")

# Creats a ui containing navbar, fluid page layout.
ui <- tagList(
  navbarPage(theme  =  shinythemes::shinytheme("united"), "Global Terrorism Database", # adds a shiny theme and a crates a navbar page
             tabPanel("Introduction"), # Creates a Tab
             tabPanel("Data Visualization", # Creates a Tab
                      fluidPage( # creates a fluid page layout
                        fluidRow( # Creates a fluid row with columns
                          column(width =  4, textInput('iso3', 'ISO3', value = "AFG")), # displays a text input box to select country
                          column(width = 4, sliderInput("year", "Year range:", min = 1970, max = 2015, value = c(1970,2015))), # displays a slider to select year range
                          div(plotlyOutput("graph")) # craetes a division and displays a map
                        ),
                        
                        tags$hr(style="border-color: black;"), # adds a horizontal line with black colour
                        
                        fluidRow( # creates a fluid row layout with columns
                          column(width = 4, selectInput("type.select", 'Select Attack Type', choices = c()), # displays a drop down list to select attack type;no multiples allowed
                                 plotlyOutput("type.pie")), # displays a interactive plot 
                          column(width = 4, selectInput("target.select",'Select Target Type', choices = c()), # displays a drop down list to select target type;no multiples allowed 
                                 plotlyOutput("target.pie")), # displays a interactive plot
                          column(width = 4, selectInput("weap.select", 'Select Weapon Type',choices = c()), # displays a drop down list to select weapon type;no multiple allowed
                                 plotlyOutput("weap.pie")) # displays a interactive plot
                        ),
                        
                        tags$hr(style="border-color: black;"), # displays a horizontal line with black colour.
                        
                        fluidRow( # Creates a fluid row with column layout
                          column(width = 4,plotlyOutput("plot.multiple")), # displays a interactive plot
                          column(width = 4,plotlyOutput("plot.success")), # displays a interactive plot
                          column(width = 4,plotlyOutput("plot.suicide")) # displays a interactive plot 
                        ))
             )))


# Loads the shinyUi app
shinyUI(ui)