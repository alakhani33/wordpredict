##This module consists of two functions that are invoked by server.R to predict and output the next word.
##transitionMatrix.RData and averageTransitionMatrix.RData produced by FinalPred.R are an input to this module.
##Ideas and inspiration for several segments in this module came from exceptional learners/developers, such as dormantroot, tomfonte, ivanliu, and others.

##This function is responsible for preprocessing the user-provided input.
preprocessUserInput = function(userInput, bleepers) {

##Build the corpus for the input provided by the user
userInputCorpus = Corpus(VectorSource(userInput))
  
##Preprocess Step #1: Remove non-ASCII characters from the input
##Reference: http://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
##Reference: http://stackoverflow.com/questions/18153504/removing-non-english-text-from-corpus-in-r-using-tm
removeNonASCII <-
  content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))

userInputCorpus = tm_map(userInputCorpus,
                           removeNonASCII,
                           mc.cores=1)

##Preprocess Step #2: Remove all punctuations with the exception of apostrophes  
##Reference: http://stackoverflow.com/questions/14281282/ re: how-to-write-custom-removepunctuation-function-to-better-deal-with-unicode-cha
##Reference: http://stackoverflow.com/questions/8697079/remove-all-punctuation-except-apostrophes-in-r
customRemovePunctuation <- content_transformer(function(x) {
  x <- gsub("[[:punct:]]"," ",tolower(x))
  return(x)
})

userInputCorpus = tm_map(userInputCorpus,
                           customRemovePunctuation,
                           mc.cores=1)

##Preprocess Step #3: Remove numbers
userInputCorpus = tm_map(userInputCorpus,
                           removeNumbers,
                           mc.cores=1)
  
##Preprocess Step #4: Remove white spaces
userInputCorpus = tm_map(userInputCorpus,
                           stripWhitespace,
                           mc.cores=1)
  
##Preprocess Step #5: Remove bleeper terms
userInputCorpus = tm_map(userInputCorpus,
                           removeWords,
                           bleepers,
                           mc.cores=1)
  
predAppinput =    unlist(str_split(as.character(userInputCorpus[[1]])," "))
  
predAppinput = predAppinput[predAppinput != ""]
  
return(predAppinput)
}


##This function makes a word prediction using the Markov chain approach 
predAppfunction <- function(preprocessedInput,
                            Terms2Predict,
                            nextwordPredictor) {

##Initiatize and assign
textPrediction <- list()
textPrediction$stateHistory <- character()
numberWords <- length(preprocessedInput)
curState <- preprocessedInput[1]
vocabulary <- states(nextwordPredictor)
  
##If the first word is not in vocabulary, create a random index
##floor takes a single numeric argument x and returns a numeric vector containing the largest integers not greater than the corresponding elements of x.
if (!curState %in% vocabulary) {
  randomIdx <- floor(length(vocabulary) * runif(1)) + 1
  curState <- vocabulary[randomIdx]
}

textPrediction$stateHistory <- 
  append(textPrediction$stateHistory, curState)
  
##Look up the probability of the each given word in the user provided input and build the state table
for (n in seq(2,numberWords)) {
  nextState <- preprocessedInput[n]
  if (!nextState %in% vocabulary) {
    curConditionalProbability <- 
      conditionalDistribution(nextwordPredictor, curState)
      
##Output the state that has the maximum probability
nextState <- names(which.max(curConditionalProbability))
      
##In case of a tie, choose randomly
if (length(nextState) > 1) {
    randomIdx <- floor(length(nextState) * runif(1)) + 1
    nextState <- nextState[randomIdx]
  }
}
curState <- nextState
    
textPrediction$stateHistory <- 
  append(textPrediction$stateHistory, curState)
}
  
textPrediction$conditionalProbability <- 
  sort(conditionalDistribution(nextwordPredictor, curState),
        decreasing=FALSE)[1:Terms2Predict]
  
return(textPrediction)
}