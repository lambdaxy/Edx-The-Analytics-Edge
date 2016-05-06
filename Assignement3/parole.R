parole_dataset = read.csv("parole.csv")
sum(parole_dataset$violator)

parole_dataset$crime = as.factor(parole_dataset$crime)
parole_dataset$state = as.factor(parole_dataset$state)

set.seed(144)
library(caTools)
split = sample.split(parole_dataset$violator, SplitRatio = 0.7)
train = subset(parole_dataset, split == TRUE)
test = subset(parole_dataset, split == FALSE)

model = glm(violator ~ . , data=train, family=binomial)
summary(model)

predictions = predict(model, newdata = test, type = "response")
max(predictions)
confusion_matrix = table(test$violator, predictions >= 0.5)
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(test)
sensitivity = confusion_matrix[2,2] / sum(confusion_matrix[2,])
specificity = confusion_matrix[1,1] / sum(confusion_matrix[1,])
baseline_accuracy = (nrow(test) - sum(test$violator)) / nrow(test)

library(ROCR)
