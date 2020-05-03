library(dplyr)
library(rgdal)
library(ggplot2)
library(plotly)
library(scales)
library(wordcloud)
library(tm)

Airbnb<-read.csv("AB_NYC_2019.csv",sep=",",dec=".")

#See how many uniques listings we have
length(unique(Airbnb$id)) 
#Each row is a listing

#Filter those listings with availability_365=0 & number_of_reviews=0

table(Airbnb$availability_365,Airbnb$number_of_reviews) #4845 listings have 0 availability and 0 reviews

Airbnb_2 <-Airbnb %>% filter((availability_365==0 & number_of_reviews>0) |availability_365>0)
table(Airbnb_2$availability_365,Airbnb_2$number_of_reviews)

#Also, filter those with Price=0 (10 cases)
table(Airbnb_2$price)

Airbnb_2<-Airbnb_2 %>% filter(price>0) #44040 cases

#Change variable names
New_names<-c("ID","Listing_name","Host_ID","Host_Name","Boroughs","Neighbourhood",
                      "latitude","longitude","Room_type","Price_USD","Minimum_nights",
                      "Number_of_reviews","Last_review","Reviews_per_month",
             "Calculated_host_listings_count","Availability_365")
colnames(Airbnb_2)<-New_names

#Save data frame
save(Airbnb_2, file="Airbnb.RData")

#Load Rdata file
load(file="Airbnb.RData")

#See how many host have more than one listing:
T1<-Airbnb_2 %>% select(Host_ID) %>% group_by(Host_ID) %>% summarise(Count=n())
length(T1$Host_ID) #33079 hosts
median(T1$Count)
mean(T1$Count)

T2<-T1 %>% select(Count) %>% group_by(Count) %>% summarise(Number.of.host=n())
#There is one host with 327 properties!

#Finally, remove those cases with Minimum_nights>365
Airbnb_2<-Airbnb_2 %>% filter(Minimum_nights<=365) #44026 cases

#Set NAs in Negative, Neutral ans Positive to 0
Airbnb_2$Negative[is.na(Airbnb_2$Negative)] <- 0
Airbnb_2$Neutral[is.na(Airbnb_2$Neutral)] <- 0
Airbnb_2$Positive[is.na(Airbnb_2$Positive)] <- 0



save(Airbnb_2, file="Airbnb.RData")
load(file="Airbnb.RData")
#Map of prices
nyc_sf <- readOGR("geo_export_6143d480-ee3e-491f-b3da-e1a4446ae67d.shp")
row.names(nyc_sf) <- c("Queens", "Staten Island", "Bronx", "Brooklyn", "Manhattan")

#Shape of NY:
ggplot(nyc_sf) +
  aes(long,lat,group=group,fill=id) +
  geom_polygon() +
  coord_equal() 

ggplot(nyc_sf, aes(long,lat,group=group)) +
  geom_polygon(fill="gray70") +
  theme_minimal()

#Random sample of properties to plot:
sample<-sample(c(1:length(Airbnb_2$ID)), size=75, replace=F)
Airbnb_random<-Airbnb_2[sample,]

quantile(Airbnb_2$Positive,0.90)

Airbnb_nine_decile<-Airbnb_2[Airbnb_2$Positive>=quantile(Airbnb_2$Positive,0.90),]

p<-ggplot(nyc_sf, aes(long,lat,group=group)) +
  geom_polygon(fill="gray77") +
  geom_point(data=Airbnb_nine_decile,aes(x=longitude, y=latitude, group = NA, color=Positive)) +
  theme_minimal() +
  theme(legend.position="bottom",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  ggtitle("New York: 9th decile of positive reviews") +
  labs(colour="Positive \nreviews") +
  scale_color_gradient(low="#007D8C",high="#FF5A5F")


ggplotly(p,tooltip = c("colour","fill"))  

colors<-c("#FF5A5F","#007D8C","#FFAB08","#767676","mediumorchid4") 

#Using all the points from the dataset and NO NYC shape
p<-ggplot(data=Airbnb_2)+geom_point(aes(x=longitude, y=latitude, color=Price_USD))
ggplotly(p,tooltip = c("colour","fill")) 

#========================================Univariate analysis=============================
c("Boroughs","Room_type","Price_USD","Minimum_nights","Number_of_reviews")

variable<- c("Room_type")
breakdown<-"Boroughs"
Remove_outilers<-"Yes"

colors<-c("#FF5A5F","#00A699","#FFAB08","#767676","mediumorchid4")


Plot_univariate_1_df<-Airbnb_2 %>% select(ID,variable)

Plot_univariate_1_df<-if(is.numeric(Plot_univariate_1_df[,2]))
{
  if(Remove_outilers=="Yes")
  {
    Plot_univariate_1_df %>% 
      filter(Plot_univariate_1_df[,2]<quantile(Plot_univariate_1_df[,2],0.95))

  }else{
    Plot_univariate_1_df
  }
    
}


if (is.numeric(Plot_univariate_1_df[,2])==TRUE)
{
  p<-ggplot(Plot_univariate_1_df, aes(y=Plot_univariate_1_df[,2])) + 
    geom_boxplot(fill="#007D8C",outlier.shape = 21, outlier.fill = "firebrick") +
    ggtitle(paste("Boxplot of",names(Plot_univariate_1_df[2]))) + 
    ylab(names(Plot_univariate_1_df[2])) +
    theme_minimal()
  
  ggplotly(p)
  
}else{
  Categorical_univariate_1_df<- Plot_univariate_1_df %>% group_by(Group=Plot_univariate_1_df[,2]) %>% 
    summarise(Count=n()) %>% mutate(Perc=round(Count/sum(Count),2))
  
  p<-ggplot(Categorical_univariate_1_df, aes(x=NA,y=Perc, fill=Group)) + 
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label=percent(Perc)),position=position_stack(vjust=0.5), color = "gray20", size = 4.5) +
    ggtitle(paste("Relative frequency of",names(Plot_univariate_1_df[2]))) +
    ylab("Percentage") +
    scale_fill_manual(values=colors) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text= element_blank(),
          legend.position="bottom")
  
  ggplotly(p,tooltip = c("y","fill"))
}

#Now with breakdowns (Boroughs or Room_type)
c("Boroughs","Room_type","Price_USD","Minimum_nights","Number_of_reviews")

variable<- c("Price_USD")
breakdown<-c("Boroughs")

Plot_univariate_2_df<-Airbnb_2 %>% select(ID,variable,breakdown)

p<-ggplot(Plot_univariate_2_df, aes(x=Plot_univariate_2_df[,3], y=Plot_univariate_2_df[,2])) +
  geom_boxplot(aes(color=Plot_univariate_2_df[,3])) +
  ggtitle(paste("Boxplot of",names(Plot_univariate_2_df[2]),"by",names(Plot_univariate_2_df[3]))) +
  xlab(names(Plot_univariate_2_df[3])) +
  ylab(names(Plot_univariate_2_df[2])) +
  scale_color_manual(names(Plot_univariate_2_df[3]),values=colors) +
  theme_minimal() 

ggplotly(p)

variable<- c("Room_type")
breakdown<-c("Boroughs")

Plot_univariate_2_df<-Airbnb_2 %>% select(ID,variable,breakdown)

Categorical_univariate_2_df <- Plot_univariate_2_df %>%
  select(names(Plot_univariate_2_df[2]),names(Plot_univariate_2_df[3]))  %>% 
  group_by_all() %>% 
  summarise(Count = n()) %>% 
  mutate(Freq=Count/sum(Count))


Variable_names<-colnames(Categorical_univariate_2_df)

p<-ggplot(Categorical_univariate_2_df, 
          aes_string(x=Variable_names[1],y=Variable_names[4],fill=Variable_names[2])) +
  geom_bar(stat="identity") +
  geom_text(aes(label=percent(Freq)),position = position_stack(vjust = 0.5),
            color="gray20",size=4) +
  scale_fill_manual(values=colors) +
  scale_y_continuous(labels=percent) +
  ggtitle(paste("Relative frequency of",Variable_names[1],"by",Variable_names[2])) +
  xlab(Variable_names[1]) + ylab("Percentage") +
  theme_minimal()

ggplotly(p,tooltip = c("y","fill"))

#Now histograms, will only run for numeric variables
c("Boroughs","Room_type","Price_USD","Minimum_nights","Number_of_reviews")

variable<- c("Room_type")
breakdown<-c("Boroughs")

Plot_univariate_3_df<-Airbnb_2 %>% select(ID,variable,breakdown)

p<-if(is.numeric(Plot_univariate_3_df[,2]))
{
  ggplot(Plot_univariate_3_df, aes(Plot_univariate_3_df[,2])) + 
    geom_histogram(bins = length(hist(Plot_univariate_3_df[,2])$counts),
      fill="#007D8C") +
    ggtitle(paste("Histogram of",names(Plot_univariate_3_df[2]))) + 
    xlab(names(Plot_univariate_3_df[2])) +
    ylab("Count") +
    theme_minimal()
}else{
  ggplot() +
    theme_minimal()
}

p

#==============================================Sentiment analysis=============================

c("Boroughs","Room_type")

breakdown<-c("Boroughs")

Plot_Sentiment_1_df<-Airbnb_2 %>% select(breakdown,Negative,Neutral,Positive)

Sentiment_1_df<- Plot_Sentiment_1_df %>% summarise(Negatives=sum(Negative),
                                                    Neutrals=sum(Neutral),
                                                    Positives=sum(Positive)) %>% 
              gather(key="Score",value="Total",1:3) %>% 
              mutate(Perc=Total/sum(Total))

p<-ggplot(Sentiment_1_df, aes(x=NA,y=Perc, fill=Score)) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label=percent(Perc)),position=position_stack(vjust=0.5), color = "gray20", size = 4.5) +
  ggtitle("Sentimental score (percentages)") +
  scale_fill_manual(values=colors) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text= element_blank(),
        legend.position="bottom")

ggplotly(p,tooltip = c("y","fill"))

#Now with breakdowns

Plot_Sentiment_2_df<-Plot_Sentiment_1_df



Plot_Sentiment_2_df<- Plot_Sentiment_1_df %>% rename("The_breakdown"=
                                                       ifelse(names(Plot_Sentiment_1_df)[1]=="Boroughs","Boroughs","Room_type"))


Sentiment_2_df<- Plot_Sentiment_2_df %>% group_by(The_breakdown) %>% 
                                          summarise(Negatives=sum(Negative),
                                                    Neutrals=sum(Neutral),
                                                    Positives=sum(Positive)) %>% 
              gather(key="Score",value="Total",2:4) %>% 
              group_by(The_breakdown) %>% 
              mutate(Perc = Total / sum(Total)) 


p<-ggplot(Sentiment_2_df, 
          aes(x=The_breakdown,y=Perc,fill=Score)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=percent(Perc)),position = position_stack(vjust = 0.5),
            color="gray20",size=4) +
  scale_fill_manual(values=colors) +
  scale_y_continuous(labels=percent) +
  ggtitle(paste("Sentimental score by",names(Plot_Sentiment_1_df)[1])) +
  xlab(names(Plot_Sentiment_1_df)[1]) + ylab("Percentage") +
  theme_minimal()

ggplotly(p,tooltip = c("y","fill"))

#========================================Multivariate analysis=============================

c("Boroughs","Room_type","Price_USD","Minimum_nights","Number_of_reviews")

variable_1<- c("Price_USD")
variable_2<- c("Number_of_reviews")
breakdown<-c("Boroughs")

Plot_multivariate_1_df<-Airbnb_2 %>% select(variable_1,variable_2,breakdown) 

#Removing outliers:
Plot_multivariate_1_df_2 <- Plot_multivariate_1_df %>% 
  filter(Plot_multivariate_1_df[,1]<quantile(Plot_multivariate_1_df[,1],0.95) &
         Plot_multivariate_1_df[,2]<quantile(Plot_multivariate_1_df[,2],0.95)) 


p<-ggplot(Plot_multivariate_1_df_2, 
       aes(x=Plot_multivariate_1_df_2[,1],y=Plot_multivariate_1_df_2[,2], 
           color=Plot_multivariate_1_df_2[,3])) + 
  geom_point() +
  scale_color_manual(values=colors) +
  ggtitle(paste(names(Plot_multivariate_1_df_2)[1], "Vs.",names(Plot_multivariate_1_df_2)[2],
                "by",names(Plot_multivariate_1_df_2)[3])) +
  xlab(names(Plot_multivariate_1_df_2)[1]) +
  ylab(names(Plot_multivariate_1_df_2)[2])+
  labs(colour=names(Plot_multivariate_1_df_2)[3]) +
  theme_minimal()

ggplotly(p)

#Correlation matrixes

c("Price_USD","Minimum_nights","Number_of_reviews","Availability_365")

Multivariate_corr<- Airbnb_2 %>% select(Price_USD,Minimum_nights,Number_of_reviews,Availability_365) %>% 
  filter(Price_USD<=quantile(Airbnb_2$Price_USD,0.95)) %>% rename("Price"="Price_USD",
                                                                  "Min nights"="Minimum_nights",
                                                                  "# Reviews"="Number_of_reviews",
                                                                  "Availability"="Availability_365")

library(corrplot)
corr<-cor(Multivariate_corr)
corrplot(corr, method="number",
         col=colorRampPalette(c("#FF5A5F","#FC642D","#00A699"))(100), 
         tl.col="black", 
         tl.srt=45,
         title="Correlation for NY listing variables",
         mar = c(1,1,2,1),
         number.cex = 0.85,
         cl.align.text="l")


#Correlations for sentiment analysis

Multivariate_corr_2<- Airbnb_2 %>% select(Price_USD,Availability_365,Negative,Positive) %>% 
  filter(Price_USD<=quantile(Airbnb_2$Price_USD,0.95)) %>% rename("Price"="Price_USD",
                                                                  "Availability"="Availability_365",
                                                                  "Negative rev."="Negative",
                                                                  "Positive rev."="Positive")
  

corr_2<-cor(Multivariate_corr_2)
corrplot(corr_2, method="number",
         col=colorRampPalette(c("#FF5A5F","#FC642D","#00A699"))(100), 
         tl.col="black", 
         tl.srt=45,
         title="Correlation for NY Reviews",
         mar = c(1,1,2,1),
         number.cex = 0.85,
         cl.align.text="l")

#Wordcloud positive

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

matrixPos <- as.matrix(dtmPos) 
wordsPos <- sort(rowSums(matrixPos),decreasing=TRUE) 
freqTablePos <- data.frame(word = names(wordsPos),freq=wordsPos)
head(freqTablePos, 40)


layout(matrix(c(1,2),nrow = 2),heights=c(1,4))
par(mar=rep(0,4))
plot.new()
text(x=0.5,y=0.5,"Most Used Words in Positive Reviews")
wordcloud(words = freqTablePos$word, freq = freqTablePos$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


#Reviews negative
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

#Wordcloud Negative
layout(matrix(c(1,2),nrow = 2),heights=c(1,4))
par(mar=rep(0,4))
plot.new()
text(x=0.5,y=0.5,"Most Used Words in Negative Reviews")
wordcloud(words = freqTableNeg$word, freq = freqTableNeg$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

