#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries
library(shiny)
library(textclean)
library(tm)
library(stringr)
library(lexicon)
library(tokenizers)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Word Prediction"),
    
    # Sidebar with a text field input and a button to click and run the code

    sidebarLayout(
        sidebarPanel(
            textInput("userInput", "User Text Input"),
            sliderInput("numWords", "Number of Words to Predict", 1,10,1),
            actionButton("userRun", "Submit"),
            actionButton("resetAll", "Reset all")
        ),
            
    # Main Panel with a word prediction
        
    mainPanel(
        h4("ShinyApp - Predicting Words: Presentation"),
        p("In this App you will get a selected number of predicted words for your input. If the App doesn't get a prediction you will get a message saying:",
          div("No prediction for this input, try another one.", style = "color:blue")),
        p("When the",
          strong("Submit"),
          "button is pressed the code will run. You can use the slider to select how many words/hypothesis you want to predict. Always press",
          strong("Submit"),
          "after changing a parameter - number of words or the input text"),
        p("The",
          strong("Reset"),
          "button clears all inputed data and outputed predictions"),
        p("Enjoy!"),
        em("Note: the prediction is based in clean data without stop words and profanity, so it is not expected that this App will predict this kind of words."),
        p(),
        h3("Next word prediction:"),
        textOutput("text"),
        tags$head(tags$style("#text{color: green;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
        )
        )
        )
        
    )

))
