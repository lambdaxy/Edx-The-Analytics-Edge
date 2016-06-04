library(mice)
library(ggplot2)
library(rpart)
library(mice)
library(klaR)

# Load Data
train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

# Data Cleaning ##################################################################################

# Remove YOB outliers
train = train[train$YOB >= 1900 & train$YOB <= 2003,]

# Replacing empty strings with NA in variables other than questions
train$Income[train$Income == ""] = NA
train$Income = factor(train$Income)
train$Gender[train$Gender == ""] = NA
train$Gender = factor(train$Gender)
train$EducationLevel[train$EducationLevel == ""] = NA
train$EducationLevel = factor(train$EducationLevel)
train$HouseholdStatus[train$HouseholdStatus == ""] = NA
train$HouseholdStatus = factor(train$HouseholdStatus)

test$Income[test$Income == ""] = NA
test$Income = factor(test$Income)
test$Gender[test$Gender == ""] = NA
test$Gender = factor(test$Gender)
test$EducationLevel[test$EducationLevel == ""] = NA
test$EducationLevel = factor(test$EducationLevel)
test$HouseholdStatus[test$HouseholdStatus == ""] = NA
test$HouseholdStatus = factor(test$HouseholdStatus)

sum(is.na(train$Income))/nrow(train)          # 18.5%
sum(is.na(train$Gender))/nrow(train)          # 2%
sum(is.na(train$EducationLevel))/nrow(train)  # 15.5%
sum(is.na(train$HouseholdStatus))/nrow(train) # 8%

sum(is.na(test$Income))/nrow(test)          # 17.6%
sum(is.na(test$Gender))/nrow(test)          # 2.1%
sum(is.na(test$EducationLevel))/nrow(test)  # 14.4%
sum(is.na(test$HouseholdStatus))/nrow(test) # 7.3%

ggplot(data = train, mapping = aes(x = YOB)) + geom_bar()
ggplot(data = train, mapping = aes(x = Gender)) + geom_bar()
ggplot(data = train, mapping = aes(x = Income)) + geom_bar()
ggplot(data = train, mapping = aes(x = HouseholdStatus)) + geom_bar()
ggplot(data = train, mapping = aes(x = EducationLevel)) + geom_bar()

ggplot(data = train, mapping = aes(x = Party)) + geom_bar()

# Hand picked questions
#109244 Are you a feminist? Yes,No
#114152 Do you support a particular charitable cause with a lot of your time and/or money? Yes,No
#115611 Do you personally own a gun? Yes,No
#118232 Are you more of an idealist or a pragmatist? Idealist,Pragmatist

# Lots of young people single with no kids. Data is very much skewed
# Some clear outliers in the YOB variable

table(train$Income,train$HouseholdStatus)

kModesInout = train[,8:108]
testKmodes = kmodes(data=kModesInout, 5)

# Input Income with rpart
miceMod = mice(train[, !names(train) %in% "Party"], method="rf")
miceOutput <- complete(incomeMod)

# Test GLM (test again since first way of setting na was not good.)
SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)
PredTest = predict(SimpleMod, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, PREDICTIONS = PredTestLabels)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)