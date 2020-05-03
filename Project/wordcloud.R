#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("wordcloud2")
library("wordcloud2")
#install.packages("NLP")
library(NLP)
#install.packages("tm")
library(tm)
#install.packages("dplyr")
library(dplyr)


##Read the Data
load(file="Reviews_clean.RData")

#Select a random sample
sample<-sample(c(1:length(Reviews_2$id)), size=10000, replace=F)
Reviews_random<-Reviews_2[sample,]
#View(Reviews_random)

#Create a vector containing only the text

# Create a corpus  
docs <- Corpus(VectorSource(Reviews_random$comments))
#inspect(docs)
#clean data with tm
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
#View(docs)

#doc term-matrix
dtm <- TermDocumentMatrix(docs) 
#View(dtm)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
freqTable <- data.frame(word = names(words),freq=words)
head(freqTable, 40)


barplot(freqTable[1:20,]$freq, las = 2, names.arg = freqTable[1:20,]$word,
        ylab = "Word frequencies",
        col = brewer.pal(5, "Pastel1"),
        main ="Most frequent words")

#word cloud
#set.seed(1234) # for reproducibility 
wordcloud(words = freqTable$word, freq = freqTable$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Select reviews
load(file="Reviews_score.RData")

Negative_reviews<-Reviews_2 %>% filter(Sentimental_score=="Negative")
Positive_reviews<-Reviews_2 %>% filter(Sentimental_score=="Positive")

#Create a vector containing only the text

# Create a corpus  
docs <- Corpus(VectorSource(Negative_reviews$comments))
#inspect(docs)
#clean data with tm
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
#View(docs)

#doc term-matrix
dtm <- TermDocumentMatrix(docs) 
#View(dtm)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
freqTable <- data.frame(word = names(words),freq=words)
head(freqTable, 40)


barplot(freqTable[1:20,]$freq, las = 2, names.arg = freqTable[1:20,]$word,
        ylab = "Word frequencies",
        col = brewer.pal(5, "Reds"),
        main ="Most frequent words")

#word cloud
#set.seed(1234) # for reproducibility 
wordcloud(words = freqTable$word, freq = freqTable$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(15, "Reds"))
#Select a random sample
sampleP<-sample(c(1:length(Positive_reviews$Sentimental_score)), size=10000, replace=F)
Reviews_randomP<-Positive_reviews[sampleP,]
#Create a vector containing only the text

# Create a corpus  
docs <- Corpus(VectorSource(Reviews_randomP$comments))
#inspect(docs)
#clean data with tm
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
#View(docs)

#doc term-matrix
dtm <- TermDocumentMatrix(docs) 
#View(dtm)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
freqTable <- data.frame(word = names(words),freq=words)
head(freqTable, 40)


barplot(freqTable[1:20,]$freq, las = 2, names.arg = freqTable[1:20,]$word,
        ylab = "Word frequencies",
        col = brewer.pal(5, "Greens"),
        main ="Most frequent words")

#word cloud
#set.seed(1234) # for reproducibility 
wordcloud(words = freqTable$word, freq = freqTable$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(20, "Greens"))
