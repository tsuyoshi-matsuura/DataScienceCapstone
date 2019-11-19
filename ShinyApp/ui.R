library(shiny)
library(shinythemes)
library(tidyverse)
library(tidytext)


shinyUI(navbarPage("Next Word Prediction",
                   
                   theme = shinytheme("spacelab"),
                   
                   #Tab 1
                   tabPanel(p(icon("wrench"), "Next word app"),
                            
                            textInput("user_input", "Please enter some text:", value =""),

                            tags$hr(),

                            h4("Possible next word: "),
                            textOutput("guess")
                   ),
                   # Tab 2
                   tabPanel(p(icon("book"), "Documentation"),
                            h3("Purpose of the app"),
                            h5("The purpose of the app is to predict the 'Next Word'
                               based on the text provided by the user."),
                            tags$hr(),
                            h3("Usage"),
                            h5("Provide some text in the designated input box. The app will
                               generate a 'Next Word' that could follow the text. If the user
                               continues typing text, the predicted word will be updated."),
                            tags$hr(),
                            h3("Technical background"),
                            h5("The app is based on a quad-gram language model with Kneser-Ney smoothing. 
                               The app was trained on 90% of the provided English blogs, news and twitter text.
                               To keep the size of the app manageable and
                               the response time reasonable the generated n-grams were pruned."),
                            h5("The prediction accuracy is expected to be around 15% based on a test using the remaining 10%
                               of the blogs, news and twitter data.")
                   )
))