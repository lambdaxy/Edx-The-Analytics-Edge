library(mice)
library(ggplot2)

# Load Data
train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

# Data Cleaning ##################################################################################

# Replacing empty strings with NA in variables other than questions
train$Income[train$Income == ""] = NA
train$Gender[train$Gender == ""] = NA
train$EducationLevel[train$EducationLevel == ""] = NA
train$HouseholdStatus[train$HouseholdStatus == ""] = NA

test$Income[test$Income == ""] = NA
test$Gender[test$Gender == ""] = NA
test$EducationLevel[test$EducationLevel == ""] = NA
test$HouseholdStatus[test$HouseholdStatus == ""] = NA

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

# Lots of young people single with no kids. Data is very much skewed
# Some clear outlies in the YOB variable

table(train$Income,train$HouseholdStatus)

