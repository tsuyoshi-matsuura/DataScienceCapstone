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
                            h3("Purpose of the tool"),
                            h5("The purpose of the tool is to predict up to five words
                               that could follow the text provided by the user."),
                            tags$hr(),
                            h3("Usage"),
                            h5("Provide some text in the designated input box. The tool will
                               generate up to five words that could follow the text. If the user
                               continues typing text, the predicted words will be updated."),
                            tags$hr(),
                            h3("Technical background"),
                            h5("The tool is based on a quad-gram language model with Kneser-Ney smoothing. 
                               The tool was trained on 90% of the English blogs, news and twitter text
                               that was provided by the course. To keep the size of the app manageable and
                               the response time reasonable the generated n-grams were pruned."),
                            h5("The prediction accuracy is expected to be around 15% based on tests using the remaining 10%
                               of the blogs, news and twitter data.")
                   )
))