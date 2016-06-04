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

threshold = 0.5
inputedPrediction = predict(glmInputed, newdata=trainTestInputed, type="response")
confusionMatrix = table(trainTestInputed$Party, inputedPrediction > threshold)
inputedAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestInputed)
notInputedPrediction = predict(glmNotInputed, newdata=trainTestNoyInputed, type="response")
confusionMatrix = table(trainTestNotInputed$Party, notInputedPrediction > threshold)
notInputedAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestNotInputed)

# Results are very similar with and without inputed data based on a logistic regression model.
# InputedAccuracy = 62.0%
# NotInputedAccuracy = 62.2%

trainInputedGender = subset(train, select = -c(IIncome, Gender, IHouseholdStatus, IEducationLevel))
trainTrainInputedGender = subset(trainInputedGender, spl==TRUE)
trainTestInputedGender = subset(trainInputedGender, spl==FALSE)
glmInputedGender = glm(Party ~ . -USER_ID, data=trainTrainInputedGender, family=binomial)
inputedGenderPrediction = predict(glmInputedGender, newdata=trainTestInputedGender, type="response")
confusionMatrix = table(trainTestInputedGender$Party, inputedGenderPrediction > threshold)
inputedGenderAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestInputedGender)

# inputedGenderAccuracy = 62.4%

trainInputedIncome = subset(train, select = -c(Income, IGender, IHouseholdStatus, IEducationLevel))
trainTrainInputedIncome = subset(trainInputedIncome, spl==TRUE)
trainTestInputedIncome = subset(trainInputedIncome, spl==FALSE)
glmInputedIncome = glm(Party ~ . -USER_ID, data=trainTrainInputedIncome, family=binomial)
inputedIncomePrediction = predict(glmInputedIncome, newdata=trainTestInputedIncome, type="response")
confusionMatrix = table(trainTestInputedIncome$Party, inputedIncomePrediction > threshold)
inputedIncomeAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestInputedIncome)

# inputedGenderAccuracy = 62.3%

trainInputedHouseholdStatus = subset(train, select = -c(IIncome, IGender, HouseholdStatus, IEducationLevel))
trainTrainInputedHouseholdStatus = subset(trainInputedHouseholdStatus, spl==TRUE)
trainTestInputedHouseholdStatus = subset(trainInputedHouseholdStatus, spl==FALSE)
glmInputedHouseholdStatus = glm(Party ~ . -USER_ID, data=trainTrainInputedHouseholdStatus, family=binomial)
inputedHouseholdStatusPrediction = predict(glmInputedHouseholdStatus, newdata=trainTestInputedHouseholdStatus, type="response")
confusionMatrix = table(trainTestInputedHouseholdStatus$Party, inputedHouseholdStatusPrediction > threshold)
inputedHouseholdStatusAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestInputedHouseholdStatus)

#inputedHouseholdStatusAccuracy = 62.4%

trainInputedEducationLevel = subset(train, select = -c(IIncome, IGender, IHouseholdStatus, EducationLevel))
trainTrainInputedEducationLevel = subset(trainInputedEducationLevel, spl==TRUE)
trainTestInputedEducationLevel = subset(trainInputedEducationLevel, spl==FALSE)
glmInputedEducationLevel = glm(Party ~ . -USER_ID, data=trainTrainInputedEducationLevel, family=binomial)
inputedEducationLevelPrediction = predict(glmInputedEducationLevel, newdata=trainTestInputedEducationLevel, type="response")
confusionMatrix = table(trainTestInputedEducationLevel$Party, inputedEducationLevelPrediction > threshold)
inputedEducationLevelAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(trainTestInputedEducationLevel)

#inputedEducationLevelAccuracy = 62.4%
