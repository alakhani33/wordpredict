##transitionMatrix.RData and averageTransitionMatrix.RData produced by FinalPred.R are an input to this module.
##The functions declared in PredApp.R are an input to this file.
##Ideas and inspiration for several segments in this module came from exceptional learners/developers, such as dormantroot, tomfonte, ivanliu, and others.

options(shiny.maxRequestSize=95*1024^2)

library(shiny)
library(rJava)
library(RWeka)
library(R.utils)
library(stringi)
library(stringr)
library(shiny)
library(textcat)
library(tm)
library(markovchain)
source("./PredApp.R")

shinyServer(function(input, output, session) {
  
  
  ##Read user-provided input
  readData = reactive({  
    
    ##Ready the bleeper data
    bleepers = read.csv("./Terms-to-Block.csv", skip=4)
    bleepers = bleepers[,2]
    bleepers = gsub(",","",bleepers) 
    
    ##Read transition matrices that were created by running FinalPred.R
    load(file="./transitionMatrix.RData")
    load(file="./averageTransitionMatrix.RData")
    
    #The Markov predictor from FinalPred.R   
    nextwordPredictor =   new("markovchain",  transitionMatrix=averageTransitionMatrix[["transitionMatrix"]])
  
    # return
    return (list("bleepers" = bleepers, "markovPredict" = nextwordPredictor))     
  });
  
##Begin prediction...
  predictWord = function(updateProgress = NULL){tryCatch({
   
    # Initialize 
    updateProgress(detail = "One moment please..... Working on it!")      
    data = readData()
    
    #Preprocess the data, then use the predAppfunction to predict one term 
    currentPhrase = preprocessUserInput(input$sPhrase, data$bleepers)
    if (length(currentPhrase) > 0) {    
      textPrediction = predAppfunction(currentPhrase, 1,  data$markovPredict)        
      predictedNextWord = t(as.matrix(textPrediction$conditionalProbability))        
      rownames(predictedNextWord) = "P(term)"     
      nextWord = (colnames(predictedNextWord)[1])
    }        
    
    ##Generate the next predicted word
    return (paste("The word most likely to follow your entry is: ", toupper(toString(nextWord))))    
  }, error = function(err){    
    return (paste("Oops, something has gone awry: ", err))
  }, finally = {} 
  )}
  
  ##Render results for UI
  output$result = renderText({    
    
    ##Respond to the "predict" button click in UI
    input$predict
    
    ##Isolate all other inputs
    isolate({       
     
      progress <- shiny::Progress$new()
      progress$set(message = "", value = 0)
      
      on.exit(progress$close())
      
      ##Create a closure to update progress.
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      
      ##Find the next word
      predictWord(updateProgress)     
    }) 
    
  })
})