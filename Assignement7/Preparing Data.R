library(mice)

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

# Save data for future use
write.csv(train, file = "trainImputedDemographics.csv")
write.csv(test, file = "testImputedDemographics.csv")

