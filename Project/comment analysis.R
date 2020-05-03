#system.time(Reviews<-read.csv("Reviews_1.csv",sep=",",dec="."))
#elapsed : 995 seconds: 17 minutes

setwd("~/OneDrive - Consorzio MIP - Politecnico di Milano/Fundamentals of statistics and data visualization/Fundamentals of statistics/Test")

load(file="Reviews_Airbnb.RData")

library(cld3) #Library foe detect language: Google's Neu.Net translator

#Select first 100 to test the function detect_language()
K<-Reviews[1:100,]
K$comments<-as.character(K$comments) #needs to be character

Lan<-detect_language(K$comments)

K$Language<-Lan #It seems to work

#Apply to the whole data set

Reviews$comments<-as.character(Reviews$comments)

Language<-detect_language(Reviews$comments) #4 minutes! 9mb vector!

Reviews$Language<-Language 

#Keep only english comments

library(dplyr)

Reviews<-Reviews %>% filter(Language=="en") #Goes from 1 166 689 to 1 006 086, we drop 160k comments

#Drop some variables that we will not need: date, reviewer_id,reviewer_name and Language (as all of them are english now)
Reviews<- Reviews %>% select(-c(date, reviewer_id,reviewer_name,Language))

#Now a random sample
sample<-sample(c(1:length(Reviews$id)), size=10000, replace=F)
Reviews_random<-Reviews[sample,]

#Inner join the Boroughs: this way we will only keep those comments with a listing in the other table
load(file="Airbnb.RData")

#First with random reviews:
Reviews_2 <- Reviews_random %>% inner_join(
                                      select(Airbnb_2, ID, Boroughs), by=c("listing_id"="ID"))

#Now with the whole review data set:
Reviews_2 <- Reviews %>% inner_join(
  select(Airbnb_2, ID, Boroughs), by=c("listing_id"="ID"))

#Import this data set:

save(Reviews_2, file="Reviews_clean.RData")

load(file="Reviews_clean.RData")

#Read positive, negative words

library(readr)
library(stringr)
library(purrr)

negative <- read_file("Negative_words.csv", locale = locale(encoding = "Windows-1252")) %>% str_split("\r\n") %>% flatten_chr()
positive <- read_file("Positive_words.csv", locale = locale(encoding = "Windows-1252")) %>% str_split("\r\n") %>% flatten_chr()


#Now obtain the vector of comments: first for a random sample
sample<-sample(c(1:length(Reviews_2$id)), size=10000, replace=F)
Reviews_random<-Reviews_2[sample,]

Comments_random<-Reviews_random$comments 

#Lower case and remove punctuations

Comments_random<- str_to_lower(Comments_random)
Comments_random<- str_replace_all(Comments_random,"[[:punct:]]","")

#Sentimental score function
sentimental.score <- function(text, positive.words, negative.words) 
{
  sentimental.score <-  lapply(text,function(text, positive.words, negative.words)
  {
    # Independent words
    words = unlist(str_split(text, " "))
    # Count positive words
    positive = !is.na(match(words, positive.words))
    #  Count negative words
    negative = !is.na(match(words, negative.words))
    # Diff between positive and negative words
    score = sum(positive) - sum(negative)
    # Se retorna el texto,puntaje y la fecha de publicación
    out <- list(text = text, score =  ifelse(score > 0,"Positive",
                                             ifelse(score == 0,"Neutral","Negative")))
    return(out)
  }, 
  positive.words, negative.words)
  # Convert to a data frame and format columns
  out <- data.frame(matrix(unlist(sentimental.score),ncol = 2,byrow = T),stringsAsFactors = F)
  colnames(out) <- c("text","score")
  return(out)
}

score_comments_random<- sentimental.score(Comments_random,positive,negative)

nega<-score_comments_random %>% dplyr::filter(score=="Negative")

#Now in the WHOLE dataset: may god bless this pc

Comments<-Reviews_2$comments 

#Lower case and remove punctuations

Comments<- str_to_lower(Comments)
Comments<- str_replace_all(Comments,"[[:punct:]]","")

score_comments_<- sentimental.score(Comments,positive,negative) #15:27

save(score_comments_, file="Reviews_score.RData")

Reviews_2<-cbind(Reviews_2,score_comments_$score)

colnames(Reviews_2)<-c("listing_id","id","comments","Boroughs","Sentimental_score")

save(Reviews_2, file="Reviews_score.RData")

#Count the number of positive, negative and neutral by listing:
load(file="Airbnb.RData")
load(file="Reviews_score.RData")

Reviews_summary<-Reviews_2 %>% group_by(listing_id,Sentimental_score) %>% count()

library(tidyr)

Listing_scores<-spread(data=Reviews_summary, key=Sentimental_score, value=n,fill = 0)

Airbnb_2<-Airbnb_2 %>% left_join(Listing_scores, by=c("ID"="listing_id"))

save(Airbnb_2, file="Airbnb.RData")
