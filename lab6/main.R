# Universidad del Valle de Guatemala
# Lab 6
# Data science
# Catedrática: Lynette García
# Jose Pablo Viana  16091
# Jose Martinez     15163
# Sergio Marchena   16387

install.packages("quanteda")
library(quanteda)

#Libreria para sentimientos
install.packages("sentimentr")
library(sentimentr)

#Libreria para formar n-grams
install.packages("stylo")
library(stylo)

install.packages("tm")
library(tm)

library(dplyr)

#Enlace para descripcion de columnas: https://developer.datafiniti.co/docs/product-data-schema

data<-read.csv("GrammarandProductReviews.csv")
View(head(data,10))

install.packages("sentimentr")
library(sentimentr)
library(dplyr)
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

#----------------- UTILIZACION sentimentr -----------------#


testData<-data

View(head(data,10))

testData$reviews.text<-as.character(testData$reviews.text)

hola<-subset(testData[c(8,10,16,20,21,24)])
hola$reviews.title<-as.character(hola$reviews.title)
hola$manufacturer<-as.character(hola$manufacturer)
hola$name<-as.factor(hola$name)

View(hola)

hola<-na.omit(hola)

hola$sentiment<-""
hola$postiveWords<-""
hola$negativeWords<-""

var<-sentiment(hola$reviews.text)
hola$sentiment<-var$sentiment

var2<-extract_sentiment_terms(hola$reviews.text)
hola$postiveWords<-var2$positive
hola$negativeWords<-var2$negative
# ------------------------------- TEST DE USO DE SENTIMENTR ----------------------------------------------- #
text<-hola[hola$reviews.id == 148314686, "reviews.text"]
text2<-hola[hola$reviews.id == 148314686, "reviews.title"]

sentiment_by(text, var = NULL)
sentiment(text)
extract_sentiment_terms(text)

sentiment_by(text2, var = NULL)
sentiment(text2)
extract_sentiment_terms(text2)

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
# ----------------------------------------- FIN DEL TEST -------------------------------------------------------- #

# GRAFICOS Y TABLAS

nombres<-table(data$name)
View(nombres)

usuarios<-table(data$reviews.username)
View(usuarios)

View(table(data$brand))
View(table(data$manufacturer))

View(table(data$reviews.userCity))

# ----------------------------- Análisis exploratorio ----------------------------
nombre_y_review <- data[,c("name","reviews.title","reviews.text")]

# Código utilizado para cuantificar cuantos reviews habían por artículo
# nombres_repetidos <- as.factor(nombre_y_review[,1])
# summary(factor)

#Agrupamos los datos por nombre y después unificamos todas las filas en una única que tiene todos los revies
nombre_y_review <- nombre_y_review %>%
  group_by(name) %>% summarise(reviews.text = paste(reviews.text, collapse=", "))

#Encontramos las palabras que más se repiten en todos los reviews
tblUniGrm<-data.frame(table(make.ngrams(txt.to.words(nombre_y_review[,2]), ngram.size = 1)))

#Ordenamos la tabla en orden descendente
tblUniGrm <- tblUniGrm[order(-tblUniGrm$Freq),]



# productos con las mejores reviews
l1<-subset(hola[c(2,1,7)])
View(l1)
l1<-l1[order(-l1$sentiment),]
top10l1<-head(l1,10)
top10l1<-subset(top10l1[c(1,3)])
View(top10l1)
top10l1<-top10l1[c(2,1)]
View(top10l1)
barplot(top10l1$sentiment)
