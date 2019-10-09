# Universidad del Valle de Guatemala
# Lab 6

# Jose Pablo Viana  16091
# Jose Martinez     15163
# Sergio Marchena   16387


data<-read.csv("GrammarandProductReviews.csv")
View(head(data,10))

#----------------- LIMPIEZA -----------------#
# To lower
data$reviews.text <- tolower(data$reviews.text)
data$reviews.title <- tolower(data$reviews.title)

# reemplazar caracteres
data$reviews.text <- gsub("@", "", data$reviews.text)
data$reviews.text <- gsub("#", "", data$reviews.text)
data$reviews.text <- gsub("'", "", data$reviews.text)

data$reviews.title <- gsub("@", "", data$reviews.title)
data$reviews.title <- gsub("#", "", data$reviews.title)
data$reviews.title <- gsub("'", "", data$reviews.title)

# Quitar stopwords, signos de puntuacion
stopWords <- stopwords(kind = "en")

data$reviews.text <- removeWords(data$reviews.text, stopWords)
data$reviews.text <- removePunctuation(data$reviews.text)
data$reviews.text <- removeNumbers(data$reviews.text)

data$reviews.title <- removeWords(data$reviews.title, stopWords)
data$reviews.title <- removePunctuation(data$reviews.title)
data$reviews.title <- removeNumbers(data$reviews.title)

# Quitar urls
data$reviews.text<- gsub('http\\S+\\s*', '', data$reviews.text)

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
