#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("tm")
library(NLP)
library(tm)
library(dplyr)

##Read the Data
load(file="Reviews_clean.RData")
load(file="Reviews_score.RData")

#Select a random sample
sample<-sample(c(1:length(Reviews_2$id)), size=10000, replace=F)
Reviews_random<-Reviews_2[sample,]
#View(Reviews_random)

#Reviews per Borought
ReviewsM<-Reviews_2%>% filter(Boroughs=="Manhattan")
sampleM<-sample(c(1:length(ReviewsM$id)), size=10000, replace=F)
Reviews_randomM<-ReviewsM[sampleM,]


ReviewsB<-Reviews_2%>% filter(Boroughs=="Brooklyn")
sampleB<-sample(c(1:length(ReviewsB$id)), size=10000, replace=F)
Reviews_randomB<-ReviewsB[sampleB,]

ReviewsBron<-Reviews_2%>% filter(Boroughs=="Bronx")
sampleBron<-sample(c(1:length(ReviewsBron$id)), size=10000, replace=F)
Reviews_randomBron<-ReviewsBron[sampleBron,]

ReviewsQ<-Reviews_2%>% filter(Boroughs=="Queens")
sampleQ<-sample(c(1:length(ReviewsQ$id)), size=10000, replace=F)
Reviews_randomQ<-ReviewsQ[sampleQ,]

ReviewsSI<-Reviews_2%>% filter(Boroughs=="Staten Island")


#Create a vector containing only the text

# Create a corpus  (Gral. Analysis)
docs <- Corpus(VectorSource(Reviews_random$comments))
docsM<-Corpus(VectorSource(Reviews_randomM$comments))
docsB<-Corpus(VectorSource(Reviews_randomB$comments))
docsBron<-Corpus(VectorSource(Reviews_randomBron$comments))
docsQ<-Corpus(VectorSource(Reviews_randomQ$comments))
docsSI<-Corpus(VectorSource(ReviewsSI$comments))

#inspect(docs)
#clean data with tm
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c("even","staying","will","just","well","get","can","place","apartment","also","stay","room","around","definitly","everything","one","made","'s","great"))
docs <- tm_map(docs, removeWords, stopwords("english"))

docsM <- docsM %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docsM <- tm_map(docsM, content_transformer(tolower))
docsM <- tm_map(docsM, removeWords, c("even","staying","will","just","well","get","can","place","apartment","also","stay","room","around","definitly","everything","one","made","'s","great"))
docsM <- tm_map(docsM, removeWords, stopwords("english"))

docsB <- docsB %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docsB <- tm_map(docsB, content_transformer(tolower))
docsB <- tm_map(docsB, removeWords, c("even","staying","will","just","well","get","can","place","apartment","also","stay","room","around","definitly","everything","one","made","'s","great"))
docsB <- tm_map(docsB, removeWords, stopwords("english"))

docsBron <- docsBron %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docsBron <- tm_map(docsBron, content_transformer(tolower))
docsBron <- tm_map(docsBron, removeWords, c("even","staying","will","just","well","get","can","place","apartment","also","stay","room","around","definitly","everything","one","made","'s","great"))
docsBron <- tm_map(docsBron, removeWords, stopwords("english"))

docsQ <- docsQ %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docsQ <- tm_map(docsQ, content_transformer(tolower))
docsQ <- tm_map(docsQ, removeWords, c("even","staying","will","just","well","get","can","place","apartment","also","stay","room","around","definitly","everything","one","made","'s","great"))
docsQ <- tm_map(docsQ, removeWords, stopwords("english"))

docsSI <- docsSI %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docsSI <- tm_map(docsSI, content_transformer(tolower))
docsSI <- tm_map(docsSI, removeWords, c("even","staying","will","just","well","get","can","place","apartment","also","stay","room","around","definitly","everything","one","made","'s","great"))
docsSI <- tm_map(docsSI, removeWords, stopwords("english"))

#View(docs)

#doc term-matrix
dtm <- TermDocumentMatrix(docs) 
dtmM <- TermDocumentMatrix(docsM) 
dtmB <- TermDocumentMatrix(docsB) 
dtmBron <- TermDocumentMatrix(docsBron) 
dtmQ <- TermDocumentMatrix(docsQ) 
dtmSI <- TermDocumentMatrix(docsSI) 

#View(dtm)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
freqTable <- data.frame(word = names(words),freq=words)
head(freqTable, 40)

#Manhattan
matrixM <- as.matrix(dtmM) 
wordsM <- sort(rowSums(matrixM),decreasing=TRUE) 
freqTableM <- data.frame(word = names(wordsM),freq=wordsM)
head(freqTableM, 40)

#Brooklyn
matrixB <- as.matrix(dtmB) 
wordsB <- sort(rowSums(matrixB),decreasing=TRUE) 
freqTableB <- data.frame(word = names(wordsB),freq=wordsB)
head(freqTableB, 40)

#Bronx
matrixBron <- as.matrix(dtmBron) 
wordsBron <- sort(rowSums(matrixBron),decreasing=TRUE) 
freqTableBron <- data.frame(word = names(wordsBron),freq=wordsBron)
head(freqTableBron, 40)

#Queens
matrixQ <- as.matrix(dtmQ) 
wordsQ <- sort(rowSums(matrixQ),decreasing=TRUE) 
freqTableQ <- data.frame(word = names(wordsQ),freq=wordsQ)
head(freqTableQ, 40)

#Staten Island
matrixSI <- as.matrix(dtmSI) 
wordsSI <- sort(rowSums(matrixSI),decreasing=TRUE) 
freqTableSI <- data.frame(word = names(wordsSI),freq=wordsSI)
head(freqTableSI, 40)


barplot(freqTable[1:20,]$freq, las = 2, names.arg = freqTable[1:20,]$word,
        ylab = "Word frequencies",
        col = brewer.pal(6, "Dark2"),
        main ="Most frequent words")

#word cloud
#set.seed(1234) # for reproducibility
x11()
wordcloud(words = freqTable$word, freq = freqTable$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Wordcloud Manhattan (Randoom Smaple)
x11()
wordcloud(words = freqTableM$word, freq = freqTableM$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Wordcloud Brooklyn (Randoom Smaple)
x11()
wordcloud(words = freqTableB$word, freq = freqTableB$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Wordcloud Bronx (Randoom Smaple)
x11()
wordcloud(words = freqTableBron$word, freq = freqTableBron$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Wordcloud Queens (Randoom Smaple)
x11()
wordcloud(words = freqTableQ$word, freq = freqTableQ$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Wordcloud Queens (Staten Island)
x11()
wordcloud(words = freqTableSI$word, freq = freqTableSI$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))






#Reviews positive
ReviewPos<-Reviews_2%>% filter(Sentimental_score=="Positive")
samplePos<-sample(c(1:length(ReviewPos$id)), size=10000, replace=F)
Reviews_randomPos<-ReviewPos[samplePos,]

# Create a corpus  (Gral. Analysis)
docsPos <- Corpus(VectorSource(Reviews_randomPos$comments))

#clean data with tm
docsPos <- docsPos %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docsPos <- tm_map(docsPos, content_transformer(tolower))
docsPos <- tm_map(docsPos, removeWords, c("didnt","got","get","one","day","stay","and","the","reservation","automated","posting","days","arrival","just","will","also"))
docsPos <- tm_map(docsPos, removeWords, stopwords("english"))

dtmPos <- TermDocumentMatrix(docsPos) 


#View(dtm)
matrixPos <- as.matrix(dtmPos) 
wordsPos <- sort(rowSums(matrixPos),decreasing=TRUE) 
freqTablePos <- data.frame(word = names(wordsPos),freq=wordsPos)
head(freqTablePos, 40)

#Wordcloud Positive (Randoom Smaple)
layout(matrix(c(1,2),nrow = 2),heights=c(1,4))
par(mar=rep(0,4))
plot.new()
text(x=0.5,y=0.5,"Most Used Words in Positive Reviews",col = "green")
x11()
wordcloud(words = freqTablePos$word, freq = freqTablePos$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))




#Reviews positive
ReviewNeg<-Reviews_2%>% filter(Sentimental_score=="Negative")

# Create a corpus  (Gral. Analysis)
docsNeg <- Corpus(VectorSource(ReviewNeg$comments))

#clean data with tm
docsNeg <- docsNeg %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docsNeg <- tm_map(docsNeg, content_transformer(tolower))
docsNeg <- tm_map(docsNeg, removeWords, c("didnt","got","get","one","day","stay","and","the","reservation","automated","posting","days","arrival","just","will","also"))
docsNeg <- tm_map(docsNeg, removeWords, stopwords("english"))

dtmNeg <- TermDocumentMatrix(docsNeg) 


#View(dtm)
matrixNeg <- as.matrix(dtmNeg) 
wordsNeg <- sort(rowSums(matrixNeg),decreasing=TRUE) 
freqTableNeg <- data.frame(word = names(wordsNeg),freq=wordsNeg)
head(freqTableNeg, 40)

#Wordcloud Negative (Randoom Smaple)
x11()
layout(matrix(c(1,2),nrow = 2),heights=c(1,4))
par(mar=rep(0,4))
plot.new()
text(x=0.5,y=0.5,"Most Used Words in Negative Reviews",col = "darkred")
wordcloud(words = freqTableNeg$word, freq = freqTableNeg$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

