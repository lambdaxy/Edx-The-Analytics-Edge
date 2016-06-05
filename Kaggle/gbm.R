library(caret)

#train = read.csv("train2016.csv")
#train = subset(x = train, subset = train$YOB >= 1900 & train$YOB <= 2003) 
train = read.csv("trainImputedDemographics.csv")
train = subset(train, select = -c(Income, Gender, HouseholdStatus, EducationLevel))
#train = read.csv("trainHandPickedQuestions.csv")
train = read.csv("trainImputedDemographicsWithIndicator.csv")
train = subset(train, select = -c(Income, Gender, HouseholdStatus, EducationLevel))

set.seed(998)
inTraining <- createDataPartition(train$Party, p = .75, list = FALSE)
training <- train[ inTraining,]
testing  <- train[-inTraining,]

#dummies <- dummyVars(Party ~ . , data = training)
#trainingDummies = data.frame(predict(dummies, newdata = training))
#trainingDummies$Party = training$Party
#testingDummies = data.frame(predict(dummies, newdata = testing))
#testingDummies$Party = testing$Party

# CV tuning
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
set.seed(825)
gbmFit1 <- train(Party ~ . -USER_ID, data = training,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
predictions = predict(gbmFit1, newdata = testing, type = "prob")
confusionMatrix = table(testing$Party,predictions[,2] > 0.5)
accuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(testing)

# No tuning
fitControl <- trainControl(method = "none", classProbs = TRUE)
set.seed(825)
gbmFit4 <- train(Party ~ . -USER_ID, data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Only a single model can be passed to the
                 ## function when no resampling is used:
                 tuneGrid = data.frame(interaction.depth = 4,
                                       n.trees = 100,
                                       shrinkage = .1,
                                       n.minobsinnode = 20))
predictions = predict(gbmFit4, newdata = testing, type = "prob")
confusionMatrix = table(testing$Party,predictions[,2] > 0.5)
accuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(testing)

# tuning grid
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  classProbs = TRUE)
set.seed(825)
gbmFit3 <- train(Party ~ . -USER_ID, data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = TRUE,
                 ## Only a single model can be passed to the
                 ## function when no resampling is used:
                 tuneGrid = gbmGrid)
predictions = predict(gbmFit3, newdata = testing, type = "prob")
confusionMatrix = table(testing$Party,predictions[,2] > 0.5)
accuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(testing)

