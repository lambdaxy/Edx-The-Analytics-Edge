# Libraries
library(randomForest)

# Load Data
# Lots of missing values (better as NA or they give information none the less?)
#train = read.csv("train2016.csv", na.strings = "")
#test = read.csv("test2016.csv", na.strings = "")
train = read.csv("train2016.csv")
train[is.na(train)] = 1980
test = read.csv("test2016.csv")
test[is.na(test)] = 1980
train = train[train$YOB >= 1900 & train$YOB <= 2003,]

# Prepare Data
library(caTools)
set.seed(3000)
spl = sample.split(train$Party, SplitRatio = 0.7)
trainTrain = subset(train, spl==TRUE)
trainTest = subset(train, spl==FALSE)

# Model
rfModel = randomForest(Party ~ .-USER_ID, data = trainTrain, ntree=1000, nodesize = 200)
predictions = predict(rfModel, newdata = trainTest)

confusionMatrix = table(trainTest$Party, predictions)
accuracy = (confusionMatrix[1,1] + confusionMatrix[2,2]) / nrow(trainTest)

# using only important variables
rfModel = randomForest(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q109244 + Q115611 + Q98197 + Q101163 + Q98869 + Q113181 -USER_ID, data = trainTrain, ntree=1000, nodesize = 200)
predictions = predict(rfModel, newdata = trainTest)

confusionMatrix = table(trainTest$Party, predictions)
accuracy = (confusionMatrix[1,1] + confusionMatrix[2,2]) / nrow(trainTest)

# Submission
rfModel = randomForest(Party ~ . -USER_ID, data = train, ntree=1000, nodesize = 200)
PredTest = predict(rfModel, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, PREDICTIONS = PredTest)
write.csv(MySubmission, "SubmissionSimpleLogRF.csv", row.names=FALSE)
