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
data$reviews.text <- gsub("'", "", data$reviews.tex t)

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


################################ PREGUNTAS ##########################################

l1<-subset(hola[c(2,1,7)])
View(l1)

# productos con las mejores reviews

l1<-l1[order(-l1$sentiment),]
xu <- l1[!duplicated(l1$name),]
View(xu)
top10l1<-subset(xu[c(1,3)])
View(top10l1)
top10l1<-top10l1[c(2,1)]
View(top10l1)
top10l1<-head(top10l1,10)
top10names<-top10l1$name
top10names
barplot(top10l1$sentiment, names.arg = NA, main = "Prodcutos con mejores Reviews", xlab = "Producto", ylab = "Positivismo")
xx <- c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.4,11.5)
axis(1, at=xx, labels=top10names, tick=FALSE, las=2, line=-16, cex.axis=0.5)

# productos con las peores reviews
View(l1)
l1<-l1[order(l1$sentiment),]
xu <- l1[!duplicated(l1$name),]
View(xu)
l1<-subset(xu[c(1,3)])
top10l1<-head(l1,10)
View(top10l1)
top10names<-top10l1$name
barplot(top10l1$sentiment, names.arg = NA, main = "Prodcutos con peores Reviews", xlab = "Producto", ylab = "Negativismo")
xx <- c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.4,11.5)
axis(1, at=xx, labels=top10names, tick=FALSE, las=2, line=-20, cex.axis=0.5)
View(top10l1)


# productores con las mejores reviews
l1<-subset(hola[c(1,7)])
l1<-l1[order(-l1$sentiment),]
View(l1)

xu <- l1[!duplicated(l1$manufacturer),]
View(xu)

top10l1<-head(xu,10)
View(top10l1)
top10names<-top10l1$manufacturer
barplot(top10l1$sentiment, names.arg = NA, main = "Prodcutores con mejores Reviews", xlab = "Productor", ylab = "Positivismo")
xx <- c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.4,11.5)
axis(1, at=xx, labels=top10names, tick=FALSE, las=2, line=-9, cex.axis=0.5)
View(top10l1)

# productores con las peores reviews
l1<-subset(hola[c(1,7)])
l1<-l1[order(l1$sentiment),]
View(l1)

xu <- l1[!duplicated(l1$manufacturer),]
View(xu)

top10l1<-head(xu,10)
View(top10l1)
top10names<-top10l1$manufacturer
barplot(top10l1$sentiment, names.arg = NA, main = "Prodcutores con peores Reviews", xlab = "Productor", ylab = "Negativismo")
xx <- c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.4,11.5)
axis(1, at=xx, labels=top10names, tick=FALSE, las=2, line=-14, cex.axis=0.5)
View(top10l1)


# usuarios con mas reviews postivas
View(hola)
l1<-subset(hola[c(6,7)])
l1<-l1[order(-l1$sentiment),]
View(l1)
xu <- l1[!duplicated(l1$reviews.username),]
View(xu)
top10l1<-head(xu,10)
top10names<-top10l1$reviews.username
View(top10l1)
barplot(top10l1$sentiment, names.arg = NA, main = "Usuarios con mas Reviews Positivas", xlab = "Usuario", ylab = "Positivismo")
xx <- c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.4,11.5)
axis(1, at=xx, labels=top10names, tick=FALSE, las=2, line=-9, cex.axis=1)

# usuarios con mas reviews negativas
View(hola)
l1<-subset(hola[c(6,7)])
l1<-l1[order(l1$sentiment),]
View(l1)
xu <- l1[!duplicated(l1$reviews.username),]
View(xu)
top10l1<-head(xu,10)
top10names<-top10l1$reviews.username
View(top10l1)
barplot(top10l1$sentiment, names.arg = NA, main = "Usuarios con mas Reviews Negativas", xlab = "Usuario", ylab = "Negativismo")
xx <- c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.4,11.5)
axis(1, at=xx, labels=top10names, tick=FALSE, las=2, line=-11, cex.axis=1)


# ANALISIS DE LA EMPRESA AVEENO
# LA EMPRESA AVEENO ES LA QUE TIENE EL PROMEDIO MAS BAJO DE SENTIMIENTOS EN LOS REVIEWS DE TODAS LAS EMPRESAS, CON 0.005412462

View(hola)
empresa<-hola[hola$manufacturer == "Aveeno",]
View(empresa)
empresa<-empresa[c(1,4,5,7,8,9)]


badwords<-empresa$negativeWords
View(badwords)

delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

neg<-delete.NULLs(badwords)

View(neg)

