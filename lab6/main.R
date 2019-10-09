# Universidad del Valle de Guatemala
# Lab 6

# Jose Pablo Viana  16091
# Jose Martinez     15163
# Sergio Marchena   16387


data<-read.csv("GrammarandProductReviews.csv")
View(head(data,10))

install.packages("sentimentr")
library(sentimentr)

testData<-data

testData$reviews.text<-as.character(testData$reviews.text)

hola<-subset(testData[c(10,16,20,21)])
hola$reviews.title<-as.character(hola$reviews.title)

View(head(hola,10))

hola<-na.omit(hola)

text<-hola[hola$reviews.id == 148314686, "reviews.text"]
text2<-hola[hola$reviews.id == 148314686, "reviews.title"]

sentiment_by(text, var = NULL)
sentiment(text)
extract_sentiment_terms(text)

sentiment_by(text2, var = NULL)
sentiment(text2)
extract_sentiment_terms(text2)
