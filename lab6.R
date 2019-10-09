# LAB 6 - DATA SCIENCE

data<-read.csv("GrammarandProductReviews.csv")
View(head(data,10))

install.packages("sentimentr")
library(sentimentr)

testData<-data

testData$reviews.text<-as.character(testData$reviews.text)

testData[testData$reviews.id == 131769441,1]

sentiment_by()