library(shiny)
library(shinythemes)
library(tidyverse)
#require(markdown)
library(tidytext)


shinyUI(navbarPage("Coursera Data Science Capstone - Text Prediction",
                   
                   theme = shinytheme("spacelab"),
                   
                   #Tab 1
                   tabPanel(p(icon("calculator"), "Next word prediction"),
                            
                            textInput("user_input", "Please enter text for next word prediction:", value =""),

                            tags$hr(),

                            h4("Prediction 1"),
                            textOutput("guess1"),
                            h4("Prediction 2"),
                            textOutput("guess2"),
                            h4("Prediction 3"),
                            textOutput("guess3")
                            
                   ),
                   # Tab 2
                   tabPanel(p(icon("thumbs-o-up"), "About"),
                            h4("about.md")
                   )
))