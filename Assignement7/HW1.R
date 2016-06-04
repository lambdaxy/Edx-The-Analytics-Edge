library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling = read.csv("PollingImputed.csv")
train = subset(polling, Year == 2004 | Year == 2008)
test = subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=train, family="binomial")
testPrediction = predict(mod2, newdata=test, type="response")
testPredictionBinary = as.numeric(testPrediction > 0.5)
predictionDataFrame = data.frame(testPrediction, testPredictionBinary, test$State)

sum(predictionDataFrame$testPredictionBinary)
mean(predictionDataFrame$testPredictionBinary)

predictionDataFrame$region = tolower(predictionDataFrame$test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction))+ geom_polygon(color = "black",linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

