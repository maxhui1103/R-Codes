library(randomForest)
# install.packages('caret')
# install.packages('e1071')
library(e1071)
library(caret)
library(ggplot2)

set.seed(123) 

# dowload the dataset into R

head(data)

summary(data) #no missing data appears


#Odor and SporePrintColor are the best predictors
p = ggplot(data,aes(x=CapShape, y=CapSurface, color=Edible))
p + geom_jitter(alpha=0.3) + scale_color_manual(breaks = c('Edible','Poisonous'),values=c('darkgreen','red'))

p = ggplot(data,aes(x=StalkColorBelowRing, y=StalkColorAboveRing, color=Edible))
p + geom_jitter(alpha=0.3) + scale_color_manual(breaks = c('Edible','Poisonous'),values=c('darkgreen','red'))

p = ggplot(data,aes(x=Odor, y=SporePrintColor, color=Edible))
p + geom_jitter(alpha=0.3) + scale_color_manual(breaks = c('Edible','Poisonous'),values=c('darkgreen','red'))

p = ggplot(data,aes(x=Edible, y=Odor, color = Edible))
p + geom_jitter(alpha=0.2) + scale_color_manual(breaks = c('Edible','Poisonous'),values=c('darkgreen','red'))

p = ggplot(data,aes(x=Edible, y=SporePrintColor, color = Edible))
p + geom_jitter(alpha=0.2) + scale_color_manual(breaks = c('Edible','Poisonous'),values=c('darkgreen','red'))



#Create data for training
sample.ind = sample(2, nrow(data), replace = T, prob = c(0.05,0.95))
data.dev = data[sample.ind==1,]
data.val = data[sample.ind==2,]

#See how data sets look as edible vs poisonous
table(data$Edible)/nrow(data)
table(data.dev$Edible)/nrow(data.dev)
table(data.val$Edible)/nrow(data.val)

rf = randomForest(Edible ~ ., ntree = 100, data = data.dev)
plot(rf)
print(rf)

varImpPlot(rf, sort = T, n.var=10, main="Top 10 - Variable Importance")
#Looks like Odor is the greatest indicator


var.imp = data.frame(importance(rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]



# Predicting response variable
data.dev$predicted.response <- predict(rf , data.dev)

confusionMatrix(data = data.dev$predicted.response,
                reference = data.dev$Edible,
                positive = 'Edible')


# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
confusionMatrix(data=data.val$predicted.response,
                reference=data.val$Edible,
                positive='Edible')