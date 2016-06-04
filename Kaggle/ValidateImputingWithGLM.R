train = read.csv("trainImputedDemographics.csv")

# split train into Inputed and not Inputed subsets
TrainNotInputed = subset(train, select = -c(IIncome, IGender, IHouseholdStatus, IEducationLevel))
TrainInputed = subset(train, select = -c(Income, Gender, HouseholdStatus, EducationLevel))

# Split into training and validation sets.
library(caTools)
set.seed(3000)
spl = sample.split(train$Party, SplitRatio = 0.7)
trainTrainInputed = subset(TrainInputed, spl==TRUE)
trainTestInputed = subset(TrainInputed, spl==FALSE)
trainTrainNotInputed = subset(TrainNotInputed, spl==TRUE)
trainTestNotInputed = subset(TrainNotInputed, spl==FALSE)

glmInputed = glm(Party ~ . -USER_ID, data=trainTrainInputed, family=binomial)
glmNotInputed = glm(Party ~ . -USER_ID, data=trainTrainNotInputed, family=binomial)

threshold = 0.48
inputedPrediction = predict(glmInputed, newdata=trainTestInputed, type="response")
confusionMatrix = table(trainTestInputed$Party, inputedPrediction > threshold)
inputedAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestInputed)
notInputedPrediction = predict(glmNotInputed, newdata=trainTestNoyInputed, type="response")
confusionMatrix = table(trainTestNotInputed$Party, notInputedPrediction > threshold)
notInputedAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestNotInputed)

# Results are very similar with and without inputed data based on a logistic regression model.

