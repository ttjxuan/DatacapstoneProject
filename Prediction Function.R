setwd("~/Downloads/Course")
input_file<-file("en_US/en_US.blogs.txt",open = 'r')
linn_USblogs <-readLines(input_file); close(input_file)

library(tm)
library(wordcloud)
newSubset<- sample(linn_USblogs, size=length(linn_USblogs)*.1, replace=FALSE)
USblogs<- Corpus(VectorSource(newSubset))

USblogs <- tm_map(USblogs, removeNumbers)
USblogs <- tm_map(USblogs, removePunctuation)
USblogs <- tm_map(USblogs, removeWords,stopwords("english"))
USblogs <- tm_map(USblogs, stripWhitespace)
USblogs <- tm_map(USblogs, stemDocument)
USblogs_dtm <- DocumentTermMatrix(USblogs)
findFreqTerms(USblogs_dtm,lowfreq=20000)
dtm1 <- removeSparseTerms(USblogs_dtm, 0.99)
wordcloud(names(freq), freq, min.freq=5000, scale=c(5, .1), colors=brewer.pal(8, "Spectral"))
wordcloud(names(freq), freq, min.freq=5000, scale=c(5, 1), colors=brewer.pal(8, "Dark2"))

wordcloud(names(freq), freq, min.freq=10000, scale=c(5, .1), colors=brewer.pal(8, "Spectral"))


freq <- sort(colSums(as.matrix(dtm1)),decreasing = TRUE)
word_freq <- data.frame(word = names(freq),freq=freq)
p <- ggplot(subset(word_freq,freq>15000),aes(word,freq))
p <- p+geom_bar(stat = "identity",aes(fill = word)) +  
  theme(axis.text.x = element_text(angle=90))
p

##blogs<-sample(USblogs,size=length(USblogs)*.2, replace=FALSE)
sample_blog<-sample(USblogs,10000)

dtm3<-NGramTokenizer(USblogs,Weka_control(min=3, max=3,delimiters=" \\r\\n\\t.,;:\"()?!"))
dtm2<-NGramTokenizer(USblogs,Weka_control(min=2, max=2,delimiters=" \\r\\n\\t.,;:\"()?!"))
dtm2_backup<-dtm2
dtm3_backup<-dtm3
dtm2 <-data.frame(table(dtm2)) 
freq2<- sort(colSums(as.matrix(dtm2)),decreasing = TRUE)
fb2 <-dtm2[order(dtm2$Freq,decreasing=TRUE),]
head(fb2)
wordcloud(fb2$dtm2,fb2$Freq, min.freq=800, scale=c(3, 1), colors=brewer.pal(8, "Spectral"))
dtm3 <- data.frame(table(dtm3))
fb3 <-dtm3[order(dtm3$Freq,decreasing=TRUE),]
head(fb3)
wordcloud(fb3$dtm3, fb3$Freq, min.freq=150, scale = c(3,1), colors = brewer.pal(10, "PRGn"))

strsplit(gsub("[[:punct:]]", " ", a),"\\s+")
options(java.parameters = "-Xmx8192m")
options(java.parameters = "-Xmx16g")
library(rJava)
freq2 <- sort(colSums(as.matrix(dtm2)),decreasing = TRUE)
fb2$dtm2[1]

dtm2$field <- as.character(dtm2$dtm2)
fb2 <-dtm2[order(dtm2$Freq,decreasing=TRUE),]
noquote(fb2$field[3])
a<-fb2$field[2]
b<-strsplit(a," ")
b[[1]][2];b[[1]][1]
noquote(b[[1]][1:2])

fb3$field <- as.character(fb3$dtm3)

input <- readLines()
input <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
## first round for 3-gram
input_cut3 <-gsub("[[:punct:]]", "", input)
input_cut3 <- strsplit(input_cut3," ")
len_input3 <- length(input_cut3[[1]])
input_final3 <- noquote(input_cut3[[1]][(len_input3-1):len_input3])

len_3_gram <-dim(fb3)[1] 

target <- []
  for(i in 1:len_3_gram) {
    temp <- fb3$field[i]
    temp_word <-strsplit(temp," ")[[1]][1:2]
    if (temp_word == input_final3){
      target$name[i] = strsplit(temp," ")[[1]][3]
      target$freq[i] = fb3$Freq[i]
    }
    i = i+1
  }
if(target==[]){}


## second round for 2-gram
input_cut2 <-gsub("[[:punct:]]", "", input)
input_cut2 <- strsplit(input_cut2," ")
len_input2 <- length(input_cut2[[1]])
input_final2 <- noquote(input_cut2[[1]][(len_input2-2):len_input2])

len_2_gram <-dim(fb2)[1] 

target <- []
for(i in 1:len_2_gram) {
  temp <- fb2$field[i]
  temp_word <-strsplit(temp," ")[[1]][1]
  if (temp_word == input_final2){
    target$name[i] = strsplit(temp," ")[[1]][2]
    target$freq[i] = fb2$Freq[i]
  }
  i = i+1
}
if(target==[]){}
  