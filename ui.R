library("shiny")
library("shinythemes")


ui <- tagList(
  navbarPage( theme  =  shinythemes::shinytheme("united"), "Global Terrorism Database",
                  tabPanel("World Terrorism Map",
                            plotlyOutput("graph", width = "100%"), # displays the plot with width 100% and working on height.
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