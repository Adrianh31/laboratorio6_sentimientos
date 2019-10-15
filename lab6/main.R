# Universidad del Valle de Guatemala
# Lab 6

# Jose Pablo Viana  16091
# Jose Martinez     15163
# Sergio Marchena   16387


data<-read.csv("GrammarandProductReviews.csv")
View(head(data,10))

install.packages("sentimentr")
library(sentimentr)
library(dplyr)

testData<-data

testData$reviews.text<-as.character(testData$reviews.text)

hola<-subset(testData[c(8,10,16,20,21,24)])
hola$reviews.title<-as.character(hola$reviews.title)
hola$manufacturer<-as.character(hola$manufacturer)
hola$name<-as.factor(hola$name)

View(hola)

hola<-na.omit(hola)

text<-hola[hola$reviews.id == 148314686, "reviews.text"]
text2<-hola[hola$reviews.id == 148314686, "reviews.title"]

sentiment_by(text, var = NULL)
sentiment(text)
extract_sentiment_terms(text)

sentiment_by(text2, var = NULL)
sentiment(text2)
extract_sentiment_terms(text2)


View(head(testData))
keywords<-extract_sentiment_terms(text)

delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

pos<-delete.NULLs(keywords$positive)
pos
neg<-delete.NULLs(keywords$negative)
neg
neu<-delete.NULLs(keywords$neutral)
neu

nombres<-table(data$name)
View(nombres)

usuarios<-table(data$reviews.username)
View(usuarios)

View(table(data$brand))
View(table(data$manufacturer))

View(table(data$reviews.userCity))
