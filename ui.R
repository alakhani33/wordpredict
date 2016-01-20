##Certain variables declared in server are an input to this file.
##Ideas and inspiration for several segments in this module came from exceptional learners/developers, such as dormantroot, tomfonte, ivanliu, and others.

library(shiny)

##Define user interface for the prediction application
shinyUI(fluidPage(

  ## Title of the page
  titlePanel("Word Prediction"),
  helpText("This application is programmed to predict the next word in response to a given a word, sentence, or phrase."),

  #Left pane 
  sidebarLayout(
    sidebarPanel(      
      helpText("Enter a word, sentence, or phrase, then click SUBMIT to create a word prediction."),
      
      textInput("sPhrase", "Word, Sentence or Phrase", "I'll see you next..."),           
      br(),   
      actionButton("predict", "SUBMIT"),      
      br()   
    )
    ,

    ##Right pane
    mainPanel(
      ##Create multiple tabs
      tabsetPanel(type = "tabs", 
                  ##Tab #1: PREDICTION RESULT
                  tabPanel("PREDICTION RESULT",
                           verbatimTextOutput("result"),
                           h6('This prediction represents the word that will most likely follow the input you submitted')),
                  
                  ##Tab #2: DESCRIPTION
                  tabPanel("DESCRIPTION",
                           h6('We are well into an era of big data and small devices. As data continue to grow and user devices continue to shrink, the challenge lies in leveraging the intelligence of big data to make small form-factor devices more usable.'),
                           
                           h6('A key impediment of small devices is the lack of traditional key boards. This shortcoming can be alleviated by providing the users with as much preemptive assistance with typing as possible. This approach entails predicting words even before they are typed, based on knowledge of the historical entries.'),
                           
                           h6('The overarching goal of this project was therefore to create a knowledge-based algorithm that predicts and facilitates the data entry by users, leveraging the corpora compiled over media, such as blogs, news, and twitter feeds.  This application is not without limitations.  Given appropriate resources, several improvements could be made to improve the accuracy of prediction.')),
                  
                  ##Tab #1: How TO USE THE TOOL
                  tabPanel("HOW TO USE THE TOOL",
                           h6('Enter a word, sentence, or phrase in the field provided in the left pane.'),
                           
                           h6('Click the SUBMIT button.'),
                           
                           h6('Click on the PREDICTION RESULT tab in the right pane to find the prediction result.'),
                           
                           h6('Please note that this tool is less than perfect.  Some of its predictions may not match the results you expect.'),
                           
                           h6('This application will be continuously improved over time.')
                  )
                  
      )
    )
  )
)
)