# Load packages
library(shiny)
library(shinybusy)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("darkly"),
    titlePanel("Predict the next word"),
    sidebarPanel(
        textInput("caption", "Write a word")),
    mainPanel(
        add_busy_spinner(spin = "fading-circle"),
        tabsetPanel(type = "tabs",
                    tabPanel("all types", tableOutput("value1")),
                    tabPanel("blog", tableOutput("value2")),
                    tabPanel("twitter", tableOutput("value3")),
                    tabPanel("news", tableOutput("value4"))
       ),
  br(), 
p("This app was designed as the final project for the course Data Science Capstone, as part of the Data Science Specialization, by Johns Hopkins University", style = "font-size:10px"),
p("The project was developed by Mickey Kislev, 25.8.2021", style = "font-size:10px"),
       )
    )
    )

