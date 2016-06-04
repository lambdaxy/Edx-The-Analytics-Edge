train = read.csv("trainImputedDemographics.csv")
TrainNotInputed = subset(train, select = -c(IIncome, IGender, IHouseholdStatus, IEducationLevel))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q101162))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q106388))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q114961))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q115195))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q116953))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q116953, Q117193))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q116953, Q117193, Q120472))


library(caTools)
set.seed(3000)
spl = sample.split(train$Party, SplitRatio = 0.7)

train = subset(trainHandPickedQuestions, spl==TRUE)
test = subset(trainHandPickedQuestions, spl==FALSE)

glmHPQ = glm(Party ~ . -USER_ID, data=train, family=binomial)
threshold = 0.5
predictions = predict(glmHPQ, newdata=test, type="response")
confusionMatrix = table(test$Party, predictions > threshold)
accuracy = (confusionMatrix[1] + confusionMatrix[4]) / nrow(test)

# accuracy 61.1% Q109244, Q114152, Q115611, Q118232
# accuracy 63.9% Q109244, Q114152, Q115611, Q118232, Q98197
# accuracy 63.7% Q109244, Q114152, Q115611, Q118232, Q98197, Q101162
# accuracy 62.9% Q109244, Q114152, Q115611, Q118232, Q98197, Q106388
# accuracy 63.5% Q109244, Q114152, Q115611, Q118232, Q98197, Q114961
# accuracy 62.8% Q109244, Q114152, Q115611, Q118232, Q98197, Q115195
# accuracy 63.9% Q109244, Q114152, Q115611, Q118232, Q98197, Q116953
# => accuracy 64.1% Q109244, Q114152, Q115611, Q118232, Q98197, Q116953, Q117193 (7 hand picked questions)
# accuracy 63.4% Q109244, Q114152, Q115611, Q118232, Q98197, Q116953, Q117193, Q120472