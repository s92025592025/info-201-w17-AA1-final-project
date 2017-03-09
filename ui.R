# Loads the required library
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Loads the file
source("analysis.R")
bool.choices <- c('BOTH', 'YES', 'NO')
# Creats a ui containing navbar, fluid page layout.
ui <- tagList(
  navbarPage(theme  =  shinythemes::shinytheme("united"), "Global Terrorism Database", # adds a shiny theme and a crates a navbar page
             tabPanel("Introduction"), # Creates a Tab
             tabPanel("Data Visualization", # Creates a Tab
                      fluidPage( # creates a fluid page layout
                        div(plotlyOutput("graph")), # craetes a division and displays a map
                        fluidRow( # Creates a fluid row with columns
                          column(width =  4, selectInput("country", "country", names(All.Country.List()))), # displays a select input box to select country
                          column(width = 4, sliderInput("year", "Year range:", min = 1970, max = 2015, value = c(1970,2015))) # displays a slider to select year range
                        ),
                        tags$hr(style="border-color: black;"), # adds a horizontal line with black colour
                        
                        fluidRow( # creates a fluid row layout with columns
                          column(width = 4, selectInput("type.select", 'Select Attack Type', choices = c()), # displays a drop down list to select attack type;no multiples allowed
                                 plotlyOutput("type.pie")), # displays the pie for attack type
                          column(width = 4, selectInput("target.select",'Select Target Type', choices = c()), # displays a drop down list to select target type;no multiples allowed 
                                 plotlyOutput("target.pie")), # displays the pie for target type
                          column(width = 4, selectInput("weap.select", 'Select Weapon Type',choices = c()), # displays a drop down list to select weapon type;no multiple allowed
                                 plotlyOutput("weap.pie")) # displays they pie for weapon type
                        ),
                        
                        fluidRow( # Creates a fluid row with column layout
                          column(width = 4, selectInput("multi.select", 'Select Yes or No', choices = bool.choices),
                                 plotlyOutput("plot.multiple")), # displays the pie for wheather the attack multiple
                          column(width = 4, selectInput("success.select", 'Select Yes or No', choices = bool.choices),
                                 plotlyOutput("plot.success")), # displays the pie for wheather the attack is successful
                          column(width = 4, selectInput("suicide.select", 'Select Yes or No', choices = bool.choices),
                                 plotlyOutput("plot.suicide")) # displays the pie for wheather the attack is suicidal
                        )
                      )
             )
  )
)

# Loads the shinyUi app
shinyUI(ui)