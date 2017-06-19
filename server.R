
library(shiny)
library(tm)
library(stringr)
# setwd("~/Downloads")
quadgram <- readRDS("quadgram.RData")
trigram <- readRDS("trigram.RData")
bigram <- readRDS("bigram.RData")
# quadgram<-read.csv("quadgram.csv",header = TRUE)
msg <<- ""

## define the prediction function
word_predict<-function(input_word){
  input_cut <-gsub("[[:punct:]]", "", input_word)
  input_cut <- removeNumbers(removePunctuation(tolower(input_word)))
  input_cut<- strsplit(input_cut," ")[[1]]
  
  
  if (length(input_cut)>= 3) {
    input_cut <- tail(input_cut,3)
    if (identical(character(0),head(quadgram[quadgram$unigram == input_cut[1] & quadgram$bigram == input_cut[2] & quadgram$trigram == input_cut[3], 4],1))){
      word_predict(paste(input_cut[2],input_cut[3],sep=" "))
    }
    else {msg <- "Using 4 gram here to predicting this word"; head(quadgram[quadgram$unigram == input_cut[1] & quadgram$bigram == input_cut[2] & quadgram$trigram == input_cut[3], 4],1)}
  }
  else if (length(input_cut) == 2){
    input_cut <- tail(input_cut,2)
    if (identical(character(0),head(trigram[trigram$unigram == input_cut[1] & trigram$bigram == input_cut[2], 3],1))) {
      word_predict(input_cut[2])
    }
    else {msg<<- "Using 3 gram here to predicting this word"; head(trigram[trigram$unigram == input_cut[1] & trigram$bigram == input_cut[2], 3],1)}
 }
  else if (length(input_cut) == 1){
    if (identical(character(0),head(bigram[bigram$unigram == input_cut[1], 2],1))) {msg<<-"No match found."; head("NA",1)}
    else {msg <<- "Using 2 gram here to predicting this word"; head(bigram[bigram$unigram == input_cut[1],2],1)}
  }
}

shinyServer(function(input, output) {
   
  output$prediction <- renderPrint({
    result <- word_predict(input$inputString)
    output$text2 <- renderText({msg})
    result
    
  })
  output$text1 <- renderText({
  input$inputString});
})
