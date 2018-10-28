#library(moments)
#library(e1071)
##install.packages("naivebayes")
library(naivebayes)
##install.packages("caret")
library(caret)
##install.packages("wordcloud")
library(wordcloud)
##install.packages("tm")
library(tm)
library(e1071)

##reading csv file
data <- read.csv("twitter_gender_classifier.csv", header = T,stringsAsFactors = FALSE)

#2.1
##data processing (Gender -- 6)
##Change the irrelevant data to -1
data$gender[] <- ifelse(data$gender == "male" | data$gender == "female" | data$gender == "brand",data$gender,-1)
##Delete them
data <- data[data$gender != -1, ]

##data processing (X_trusted_judgments -- 4)
data$X_trusted_judgments <- as.numeric(data$X_trusted_judgments)

##data processing (Gender.confidence -- 7)
data$gender.confidence <- as.numeric(data$gender.confidence)
data$gender.confidence[data$gender.confidence > 1 | data$gender.confidence < 0] <- -1
data$gender.confidence[data$gender.confidence!= -1]

##data processing (profile_yn.confidence -- 9)
data$profile_yn.confidence <- as.numeric(data$profile_yn.confidence)

##data processing (link_color -- 14)
isColor <- function(x)
{
  return(x%in%colors() | grepl("^#(\\d|[a-f]){6,8}$",x,ignore.case=TRUE))
}
data$link_color <- paste("#", data$link_color, sep="")
data <- data[isColor(data$link_color),]

##data processing (sidebar_color -- 19)
data$sidebar_color <- paste("#", data$sidebar_color, sep="")
data <- data[isColor(data$sidebar_color),]

##data processing (fav_number -- 12)
data$fav_number <- as.numeric(data$fav_number)

##data processing (retweet_count -- 18)
data$retweet_count <- as.numeric(data$retweet_count)

##data processing (tweet_count -- 22)
data<- data[data$tweet_count != "",]
data <- data[!(data$tweet_count %in% data$tweet_count[grep("[^0-9]", data$tweet_count)]),]
data$tweet_count <- as.numeric(data$tweet_count)

##data processing (Description -- 11)
data$description <- gsub("[^[:alnum:][:space:]]", " ", gsub("@\\w+:", " ",  gsub("http(s)?://\\S+\\s*", " ",gsub("\\s+"," ",gsub("</?[a-zA-Z]+>", " ", data$description)))))
data$description <- gsub("\\s+"," ",data$description)
#data$description <- strsplit(data$description, ' ')

##data processing (text -- 20)
data$text <- gsub("[^[:alnum:][:space:]]", " ", gsub("@\\w+:", " ",  gsub("http(s)?://\\S+\\s*", " ",gsub("\\s+"," ",gsub("</?[a-zA-Z]+>", " ", data$text)))))
data$text <- gsub("\\s+"," ",data$text)
#data$text <- strsplit(data$text, ' ')

#2.2
##data processing (droping Unique and useless Attributes)
newdata<- data[c(2,3,4,6,7,8,9,12,14,18,19,22)]
#Continuous Number 4 7 9 12 18 22 
#Color(discrete) 14 19
#Discrete 2 3 8 
#Gender 6
#Text 11 20
rm(isColor)


##Random Sampling

set.seed(1234)
data_sample <- sample(2, nrow(newdata), replace = T, prob = c(0.8,0.2))
Train <- newdata[data_sample==1,]
Test <- newdata[data_sample==2,]
nrow(Train)
nrow(Test)
str(Train)
str(Test)


##Build the model

model <- naive_bayes(gender ~ ., data = Train, laplace = 3)
model$tables

##plot(model)

##Prediction
##On Training
p1 <- predict(model, Train)
##Test accuary rate
tab1 <- table(p1, Train$gender)
sum(diag(tab1)) / sum(tab1)
rm(p1, tab1)

##On Testing
Test <- Test[Test$link_color%in% Train$link_color & Test$sidebar_color%in% Train$sidebar_color , ]
p2 <- predict(model, Test)
##Test accuracy rate
tab2 <- table(p2, Test$gender)
sum(diag(tab2)) / sum(tab2)
rm(p2, tab2)

#2.3
model_new <- naive_bayes(gender ~ ., data = Train, laplace = 1, usekernel = T)
p1 <- predict(model_new, Train)
##Test accuary rate
tab1 <- table(p1, Train$gender)
sum(diag(tab1)) / sum(tab1)
rm(p1, tab1)

Test <- Test[Test$link_color%in% Train$link_color & Test$sidebar_color%in% Train$sidebar_color , ]
p2 <- predict(model_new, Test)
##Test accuracy rate
tab2 <- table(p2, Test$gender)
sum(diag(tab2)) / sum(tab2)
rm(p2, tab2)

#2.4.2
##WordCloud
Brand_cloud <- which(data$gender == "brand")
Male_cloud <- which(data$gender == "male")
Female_cloud<- which(data$gender == "female")
wordcloud(data_Corpus_Clean[Brand_cloud], min.freq = 40)
wordcloud(data_Corpus_Clean[Male_cloud], min.freq = 40)
wordcloud(data_Corpus_Clean[Female_cloud], min.freq = 40)

#2.4.1
##Text Analytics
data_Corpus <- Corpus(VectorSource(data$text))
print(data_Corpus)
data_Corpus_Clean <- tm_map(data_Corpus, tolower)
data_Corpus_Clean <- tm_map(data_Corpus_Clean, removeNumbers)
data_Corpus_Clean <- tm_map(data_Corpus_Clean, removePunctuation)
data_Corpus_Clean <- tm_map(data_Corpus_Clean, stripWhitespace)

data_dtm <- DocumentTermMatrix(data_Corpus_Clean)

data_Corpus2 <- Corpus(VectorSource(data$description))
data_dtm2 <- DocumentTermMatrix(data_Corpus2, control = list(
  tolower = T,
  removeNumbers = T,
  removePunctuation = T,
  stripWhitespace = T
))

#2.5
Train_Gender <- data[data_sample==1,]$gender
Test_Gender <- data[data_sample==2,]$gender

data_dtm_train <- data_dtm[data_sample==1,]
data_dtm_test <- data_dtm[data_sample==2,]

data_Corpus_Clean_train <- data_Corpus_Clean[data_sample==1]
data_Corpus_Clean_test <- data_Corpus_Clean[data_sample==2]

male <- subset(data[data_sample==1,], gender == "male")
female <- subset(data[data_sample==1,], gender == "female")
brand <- subset(data[data_sample==1,], gender == "brand")

frequent_words <- findFreqTerms(data_dtm_train,lowfreq =5)
length(frequent_words)

data_freq_word_train <- data_dtm_train[,frequent_words]
data_freq_word_test <- data_dtm_test[,frequent_words]

yes_or_no <- function(x){
  y <- factor(x, levels = c(1,2,3),labels=c("Male","Female","Brand"))
  
}
data_train <- apply(data_freq_word_train,2,yes_or_no)
data_test <- apply(data_freq_word_test,2,yes_or_no)

model2 <- naiveBayes(data_train,data[data_sample==1,]$text,laplace=1)

data_test_pred <- predict(model2, newdata = data_train)
table(data_test_pred,Train_Gender)
