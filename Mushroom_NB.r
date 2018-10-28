# This is about Naive Bayes

library(naivebayes)
library(caret)

# read dataset
mr <- read.csv("mushroom_data.csv", header = T,stringsAsFactors = FALSE)
str(mr)
mr=mr[-1]

# shuffle and split
set.seed(1234)
mr_sample <- sample(2, nrow(mr), replace = T, prob = c(0.8,0.2))
Train <- mr[mr_sample==1,]
Test <- mr[mr_sample==2,]
nrow(Train)
nrow(Test)

# build model
model <- naive_bayes(Edible ~ ., data = Train, laplace = 1)
model$tables

# prediction
p1 <- predict(model, Train)
##Train accuary rate
# confusion matrix
tab1 <- table(p1, Train$Edible)
sum(diag(tab1)) / sum(tab1)
# second method to construction confusion matrix + model statistics
confusionMatrix(tab1)
rm(p1, tab1)

p2 <- predict(model, Test)
##Test accuracy rate
tab2 <- table(p2, Test$Edible)
sum(diag(tab2)) / sum(tab2)
confusionMatrix(tab2)
rm(p2, tab2)

# build second model with different laplace transformation
model_new <- naive_bayes(Edible ~ ., data = Train, laplace = 0)

## Train set statistics
p1 <- predict(model_new, Train)
tab1 <- table(p1, Train$Edible)
sum(diag(tab1)) / sum(tab1)
confusionMatrix(tab1)

## Test set statistics
p2 <- predict(model_new, Test)
tab2 <- table(p2, Test$Edible)
sum(diag(tab2)) / sum(tab2)
confusionMatrix(tab2)

