##This module builds the corpora, extracts a training sample, and constructs probability matrices needed for word prediction.
##Ideas and inspiration for several segments in this module came from exceptional learners/developers, such as dormantroot, tomfonte, ivanliu, and others.

##Load the requisite libraries
library(tm)
library(RWeka)
library(R.utils)
library(stringi)
library(stringr)
library(markovchain)
library(data.table)

##Set the working directory
setwd("C:/Users/Murtuza Ali/Desktop/DATA_SCIENCE/JHU/CAPSTONE/Project/FINAL")

##Build the corpus
corpusPath = "./data"
wd = file.path(".", "data")
doc = Corpus(DirSource(wd))
print("Finished building corpus")

blog_txt <- doc[[1]][[1]]
news_txt <- doc[[2]][[1]]
twitter_txt <- doc[[3]][[1]]

##Set seed and draw samples of size 2000 lines from each corpus.  This sample serves as the training set.
set.seed(1001)
doc[[1]][[1]] = sample(doc[[1]][[1]],2000)
doc[[2]][[1]] = sample(doc[[2]][[1]],2000)
doc[[3]][[1]] = sample(doc[[3]][[1]],2000)

post_blog <- doc[[1]][[1]]
post_news <- doc[[2]][[1]]
post_twitter <- doc[[3]][[1]]

##Consolidate the three samples into one called "wholeSample"
temp = rbind(post_blog, post_news)
wholeSample = rbind(temp, post_twitter)

##Write out the sampled lines to appropriate text files
write(wholeSample, "./output/outputdata/wholeSample_Training.txt")
print("Finished writing sample file")

##Conserve memory by deleting interim files
rm(post_blog)
rm(post_news)
rm(post_twitter)
rm(temp)
rm(wholeSample)

##Read in the training set in binary mode and reaffirm the number of lines in the sample
Connect = file("./output/outputdata/wholeSample_Training.txt", "rb")
numLines = countLines(Connect)
close(Connect)

##Read in the training set in textual format
h_conn = file("./output/outputdata/wholeSample_Training.txt", "r", blocking=FALSE)    
sampled_text = readLines(h_conn, numLines, skipNul=TRUE)
close(h_conn)

##Preprocess Step 1: Replace + with an empty spaces
print("Started preprocessing...")
sampled_text = gsub("\\W+"," ", sampled_text) 
processedText = sampled_text

##Create the corpus
options(mc.cores=1)
tmp_doc = Corpus(VectorSource(processedText))

##Preprocess Step 2: Remove bleeper words
bleepers = read.csv("./misc/Terms-to-Block.csv", skip=4)
bleepers = bleepers[,2]
bleepers = gsub(",","",bleepers)
tmp_doc = tm_map(tmp_doc, removeWords, bleepers) 

##Preprocess Step 3: Remove non-ASCII and non-English terms
##Reference: http://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
##Reference: http://stackoverflow.com/questions/18153504/removing-non-english-text-from-corpus-in-r-using-tm
removeNonASCII <- content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))
tmp_doc = tm_map(tmp_doc, removeNonASCII)

##Preprocess Step 4: Remove punctuations
##Reference: ##http://stackoverflow.com/questions/14281282/
##Reference: ##how-to-write-custom-removepunctuation-function-to-better-deal-with-unicode-cha
##Reference: ##http://stackoverflow.com/questions/8697079/remove-all-punctuation-except-apostrophes-in-r
customRemovePunctuation <- content_transformer(function(x) {
  x <- gsub("[[:punct:]]"," ",tolower(x))
  return(x)
})
tmp_doc = tm_map(tmp_doc, customRemovePunctuation)

##Preprocess Step 5: Remove numbers
tmp_doc = tm_map(tmp_doc, removeNumbers)

##Preprocess Step 6: Remove white space
tmp_doc = tm_map(tmp_doc, stripWhitespace)

##Preprocess Step 7: Convert to lower case
convertToLowerCase = content_transformer(function(x) tolower(x))
tmp_doc = tm_map(tmp_doc, convertToLowerCase)

corp = tmp_doc
#rm(tmp_doc)

print("Finished preprocessing")

print("Started compiling unigrams...")
##Compute document matrix for the given corpus
TDM = TermDocumentMatrix(corp)

##Derive word count.  For this training set, it turns out to be 1,328,935
word_count = sum(rowSums(as.matrix(TDM)))

##Use sparcity limit of 99.9% to contain the matrix
##TDM = removeSparseTerms(TDM, 0.999)

##Compute unigrams and organize by term and corresponding frequencies 
##Reference: ##http://www.jiem.org/index.php/jiem/article/viewFile/293/252/2402
TermFreq = sort(rowSums(as.matrix(TDM)), decreasing=TRUE)
TermFreq = as.data.frame(TermFreq)
TermFreq$unigram = rownames(TermFreq)
rownames(TermFreq) = NULL
colnames(TermFreq) = c("count","unigram")
TermFreq = as.data.table(TermFreq)
setkey(TermFreq,unigram)

TermFreqCount = TermFreq$count  
names(TermFreqCount) = TermFreq$unigram  

##Probability Distribution Function of unigrams sorted:  Frequency of each term / Total Word Count
TermPDF = sort(TermFreqCount / word_count, decreasing=TRUE)

##Cumulative distribution function: Sum of relative probabilities for each unigram
TermCDF = cumsum(TermPDF)

##Using Pareto logic, retain terms that make up 80% of the cumulative distribution 
cutoff_index = which(TermCDF >= 0.80)[1]  
weightedUniterms = names(TermCDF[1:cutoff_index])
TermPDF = TermPDF[1:cutoff_index]
TermPDF = TermPDF / sum(TermPDF)

##Build a transition matrix (rows and columns containing the same terms) showing probabilities
vocabSize = length(weightedUniterms)  
transitionMatrix = matrix(numeric(vocabSize^2),
                          byrow=TRUE,
                          nrow=vocabSize,
                          dimnames=list(weightedUniterms,
                                        weightedUniterms))

print("Finished with unigrams")
##Now examine the trigrams by first extracting them
##Reference: http://stackoverflow.com/questions/17703553/trigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka

print("Started compiling trigrams...")
TrigramTokenizer = function(x) {
  RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
}

options(mc.cores=1)
trigramTDM = TermDocumentMatrix(corp,
                                control =
                                  list(tokenize = TrigramTokenizer))

##Use sparcity limit of 99.9% to contain the matrix.  Extract trigrams, there are 315.
trigramTDM = removeSparseTerms(trigramTDM, 0.999)
trigramTDM = rowSums(as.matrix(trigramTDM))
trigrams = names(trigramTDM)

##Identify and build an list of trigrams, of which all terms appear in weightedUniterms.
trigramIndex = numeric()

for (n in seq_len(length(trigrams))) { 
  triWords = unlist(str_split(trigrams[n]," "))
  
  if (sum(triWords %in% weightedUniterms) == 3) {
    trigramIndex = append(trigramIndex,n)
  }
}

weightedTriterms = trigramTDM[trigramIndex]

##Split each trigram into three distinct words
for (m in seq_len(length(weightedTriterms))) {                    
  triWords = unlist(str_split(names(weightedTriterms[m])," "))
  
  ##for each term, figure out the row and column indexes in weightedUniterms...like their x (row) and y (Column) coordinates
  for (n in seq(2,3)) {
    rowIdx = which(grepl(paste0("^",triWords[n-1],"$"),
                         weightedUniterms))
    
    colIdx = which(grepl(paste0("^",triWords[n],"$"),
                         weightedUniterms))
    
    transitionMatrix[rowIdx,colIdx] <- 
      transitionMatrix[rowIdx,colIdx] + weightedTriterms[m]
  }
}
print("Finished with trigrams")

print("Started building probability matrices...")
##Populate probabilities in the transition matrix
Least_p = 0.01/(length(weightedUniterms)-1)

for (m in seq_len(nrow(transitionMatrix))) {
  RowSum = sum(transitionMatrix[m,])
  
  if (RowSum > 0) {
    transitionMatrix[m,] = 
      transitionMatrix[m,] / RowSum
  } else {
    transitionMatrix[m,m] = 0.99
    
    n = seq_len(ncol(transitionMatrix))
    n = n[n != m]
    transitionMatrix[m,n] = Least_p
  }
}

save(file="./transitionMatrix.RData", transitionMatrix)

zeroCount <- vector('numeric',ncol(transitionMatrix))
for (n in seq_len(length(zeroCount))) {
  zeroColIdx <- which(transitionMatrix[n,] == 0)
  zeroCount[n] <- length(zeroColIdx)
  
  if (zeroCount[n] != ncol(transitionMatrix)) {
    transitionMatrix[n,] <- 
      transitionMatrix[n,] / sum(transitionMatrix[n,])
    
    if (zeroCount[n] > 0) {
      nonZeroColIdx <- which(transitionMatrix[n,] > 0)
      
      Least_p = (1 - length(nonZeroColIdx) / 
                          ncol(transitionMatrix))/zeroCount[n]
      
      nonZeroColIdx <- which(transitionMatrix[n,] > Least_p)
      
      probabilityAdjustment <- 
        (Least_p*zeroCount[n])/length(nonZeroColIdx)
      
      transitionMatrix[n,nonZeroColIdx] <- 
        transitionMatrix[n,nonZeroColIdx] - probabilityAdjustment
      
      transitionMatrix[n,zeroColIdx] <- Least_p
    }    
  }
  else {
    transitionMatrix[n,] <- 1.0/ncol(transitionMatrix)
  }    
}

print("Finished with probability matrices")

averageMatrix <- list()
averageMatrix[["zeroCount"]] <- zeroCount
averageMatrix[["transitionMatrix"]] <- transitionMatrix

averageTransitionMatrix = averageMatrix
save(file="./averageTransitionMatrix.RData", averageTransitionMatrix)

nextwordPredictor =   new("markovchain",  transitionMatrix=averageTransitionMatrix[["transitionMatrix"]])

print("Done.  Check all files.")
