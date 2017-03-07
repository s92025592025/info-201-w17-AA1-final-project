library("shiny")
library("shinythemes")
library("leaflet")
ui <- tagList(
  navbarPage( theme  =  shinythemes::shinytheme("united"), "Global Terrorism Database",
                  tabPanel("World Terrorism Map",
                            leaflet("graph"),
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE,
                            sliderInput("slider", "Year Range", 1975,2015,2001),
                              actionButton("auto", "Auto Play", class = "btn-primary")
                           )),
                  tabPanel("Summary"),
                  navbarMenu("DATA Visualization",
                             tabPanel("Pie chart"),
                             tabPanel("Bar Graph"),
                             tabPanel("Data Table"))
))

shinyUI(ui)