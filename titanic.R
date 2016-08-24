rm(list = ls())
setwd("/Users/amish913/Dropbox/kaggle/Titanic")

## Read and import data
train <- read.csv("train.csv", header = T, sep = ",")
test <- read.csv("test.csv", header = T, sep = ",")

#names(train)
#head(train)

## Preprocessing and data trimming 
train$Survived = as.factor(train$Survived)
train$Sex = as.factor(train$Sex)
train$Pclass = as.factor(train$Pclass)

# Find and replace NAs + some trimming
ind.train.age = which(is.na(train$Age) == T)
train$Age[ind.train.age] = floor(mean(train$Age[-ind.train.age]))
train$Embarked[c(62,830)] = "S"
train$Embarked = as.factor(train$Embarked)
test$Sex = as.factor(test$Sex)
test$Pclass = as.factor(test$Pclass)

ind.test.age = which(is.na(test$Age) == T)
test$Age[ind.test.age] = floor(mean(test$Age[-ind.test.age]))
ind.test.fare = which(is.na(test$Fare) == T)
test$Fare[ind.test.fare] = mean(test$Fare[-ind.test.fare])
test$Embarked = as.factor(test$Embarked)
levels(test$Embarked) <- levels(train$Embarked)


# Feature engineering: adding a new 'title' feature to datasets
train$title = rep("none", nrow(train))
test$title = rep("none", nrow(test))
train[grepl("Mr. ", train$Name), "title"] = "Mr."
test[grepl("Mr. ", test$Name), "title"] = "Mr."
train[grepl("Mrs. ", train$Name), "title"] = "Mrs."
test[grepl("Mrs. ", test$Name), "title"] = "Mrs."
train[grepl("Miss. ", train$Name), "title"] = "Miss."
test[grepl("Miss. ", test$Name), "title"] = "Miss."
train[grepl("Master ", train$Name), "title"] = "Master"
test[grepl("Master ", test$Name), "title"] = "Master"
train$title = as.factor(train$title)
test$title = as.factor(test$title)


## Binary classification using random forest model
library(randomForest)
set.seed(1234)
model.rf1 <- randomForest(Survived ~ Sex + Age + Pclass +
                            Fare + title, data = train, importance = TRUE, ntree = 1000)
model.rf1
varImpPlot(model.rf1)

# Predictions
pred.rf1 <- predict(model.rf1, test)
# Preparing submission file
submission.rf1  <- data.frame(PassengerId = test$PassengerId, Survived = pred.rf1)
# Write to csv file
write.csv(submission.rf1, file='submission_3_rf_titanic.csv', row.names=FALSE)

