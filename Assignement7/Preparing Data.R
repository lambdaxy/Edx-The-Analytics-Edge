library(mice)

###### Prepare Inputed Data ###########################################################################

train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

# Remove YOB outliers
train = subset(x = train, subset = train$YOB >= 1900 & train$YOB <= 2003) # dropped 340 rows because of YOB. Might not be optimal.

# Combine train and test for imputing
trainTest = rbind(subset(train, select=-c(Party)),test)

# Setting demographics to NA
trainTest$Income[trainTest$Income == ""] = NA
trainTest$Income = factor(trainTest$Income)
trainTest$Gender[trainTest$Gender == ""] = NA
trainTest$Gender = factor(trainTest$Gender)
trainTest$EducationLevel[trainTest$EducationLevel == ""] = NA
trainTest$EducationLevel = factor(trainTest$EducationLevel)
trainTest$HouseholdStatus[trainTest$HouseholdStatus == ""] = NA
trainTest$HouseholdStatus = factor(trainTest$HouseholdStatus)

# inputing Demographics variables
miceModel = mice(trainTest, method = "rf")

# Set Gender,Income,HouseholdStatus,EducationLevel back into original data on new columns

miceTrainTest = complete(miceModel)
miceTrain = miceTrainTest[1:5228,]
miceTest = miceTrainTest[5229:6620,]

train$IIncome = factor(miceTrain$Income)
train$IGender = factor(miceTrain$Gender)
train$IEducationLevel = factor(miceTrain$EducationLevel)
train$IHouseholdStatus = factor(miceTrain$HouseholdStatus)

test$IIncome = factor(miceTest$Income)
test$IGender = factor(miceTest$Gender)
test$IEducationLevel = factor(miceTest$EducationLevel)
test$IHouseholdStatus = factor(miceTest$HouseholdStatus)

train$HasIncome[train$Income == ""] = 0
train$HasIncome[train$Income != ""] = 1
train$HasGender[train$Gender == ""] = 0
train$HasGender[train$Gender != ""] = 1
train$HasHouseholdStatus[train$HouseholdStatus == ""] = 0
train$HasHouseholdStatus[train$HouseholdStatus != ""] = 1
train$HasEducationLevel[train$EducationLevel == ""] = 0
train$HasEducationLevel[train$EducationLevel != ""] = 1

test$HasIncome[test$Income == ""] = 0
test$HasIncome[test$Income != ""] = 1
test$HasGender[test$Gender == ""] = 0
test$HasGender[test$Gender != ""] = 1
test$HasHouseholdStatus[test$HouseholdStatus == ""] = 0
test$HasHouseholdStatus[test$HouseholdStatus != ""] = 1
test$HasEducationLevel[test$EducationLevel == ""] = 0
test$HasEducationLevel[test$EducationLevel != ""] = 1

# Save data for future use
write.csv(train, file = "trainImputedDemographicsWithIndicator.csv")
write.csv(test, file = "testImputedDemographicsWithIndicator.csv")

###### Prepare hand picked subset ###########################################################################

train = read.csv("trainImputedDemographics.csv")
test = read.csv("testImputedDemographics.csv")
TrainNotInputed = subset(train, select = -c(IIncome, IGender, IHouseholdStatus, IEducationLevel))
trainHandPickedQuestions = subset(TrainNotInputed, select = c(USER_ID, Party, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q116953, Q117193))
TestNotInputed = subset(test, select = -c(IIncome, IGender, IHouseholdStatus, IEducationLevel))
testHandPickedQuestions = subset(TestNotInputed, select = c(USER_ID, Income, Gender, HouseholdStatus, EducationLevel, Q109244, Q114152, Q115611, Q118232, Q98197, Q116953, Q117193))
write.csv(trainHandPickedQuestions, file = "trainHandPickedQuestions.csv", row.names = FALSE)
write.csv(testHandPickedQuestions, file = "testHandPickedQuestions.csv", row.names = FALSE)
