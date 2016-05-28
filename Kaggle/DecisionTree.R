# Libraries
library(rpart)
library(rpart.plot)

# Load Data
#train = read.csv("train2016.csv", na.strings = "")
#test = read.csv("test2016.csv", na.strings = "")
train = read.csv("train2016.csv")
train[is.na(train)] = 1980
test = read.csv("test2016.csv")
test[is.na(test)] = 1980

# Prepare Data
library(caTools)
set.seed(3000)
spl = sample.split(train$Party, SplitRatio = 0.7)
trainTrain = subset(train, spl==TRUE)
trainTest = subset(train, spl==FALSE)

# Model
treeModel = rpart(Party ~ . -USER_ID, data = trainTrain, method = "class")
predictions = predict(treeModel, newdata = trainTest, type = "class")

# Model with only non question variables
treeModel = rpart(Party ~ YOB + Income + HouseholdStatus + EducationLevel + Q109244 + Q101596 + Q98869 + Q98578 + Q100689 + Q115611, data = trainTrain, method="class")
predictions = predict(treeModel, newdata = trainTest, type = "class")

confusionMatrix = table(trainTest$Party, predictions)
accuracy = (confusionMatrix[1,1] + confusionMatrix[2,2]) / nrow(trainTest)
