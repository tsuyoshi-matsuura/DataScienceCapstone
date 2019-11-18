library(shiny)
library(shinythemes)
library(tidyverse)
library(tidytext)


shinyUI(navbarPage("Next Word Prediction",
                   
                   theme = shinytheme("spacelab"),
                   
                   #Tab 1
                   tabPanel(p(icon("wrench"), "Next word tool"),
                            
                            textInput("user_input", "Please enter some text:", value =""),

                            tags$hr(),

                            h4("Possible next words are: "),
                            textOutput("guess")
                   ),
                   # Tab 2
                   tabPanel(p(icon("book"), "Documentation"),
                            h3("Purpose of the tool")
                   )
))