library(foreach)

train = read.csv("train2016.csv")
test = read.csv("test2016.csv")


# Keep only questions variables
QTrain = train[,grep(pattern = "Q",x = names(train))]
QTest = test[,grep(pattern = "Q",x = names(test))]

# Add the user_id variable
QTrain$USER_ID = train$USER_ID
QTest$USER_ID = test$USER_ID

# Transform Data so that unanswerd questions contain 1 and non answered questions contains 0
toNum = function(x) {
  if (x == "") {
    return(0)
  }
  else {
    return(1)
  }
}
foreach(column = names(QTrain)) %do% if (column != "USER_ID") { QTrain[[column]] = sapply(QTrain[[column]], toNum) }
foreach(column = names(QTest)) %do% if (column != "USER_ID") { QTest[[column]] = sapply(QTest[[column]], toNum) }

# Save Questions answered data.
write.csv(QTrain, file = "trainQuestionsAnswered.csv", row.names = FALSE)
write.csv(QTest, file = "testQuestionAnswered.csv", row.names = FALSE)

# Explore the data with Hierarchical clustering.
distancesQTrain = dist(QTrain[1:101], method = "euclidean")
clusterQTrain = hclust(distancesQTrain, method = "ward") 
plot(clusterQTrain)
# seems that there might be 2 to 5 clusters. However 2 and 3 are the most likely.
distancesQTest = dist(QTest[1:101], method = "euclidean")
clusterQTest = hclust(distancesQTest, method = "ward") 
plot(clusterQTest)
# Test data gives similar results altough 2 and 3 clusters are most likely here.Let's start with three.

# Combine Train and Test data
QTrainTest = rbind(QTrain,QTest)
# Find clusters on combined data 
distancesQTrainTest = dist(QTrainTest[1:101], method = "euclidean")
clusterQTrainTest = hclust(distancesQTrainTest, method = "ward") 
plot(clusterQTrainTest)

# Cut the tree (find clusters)
clusterGroups = cutree(clusterQTrainTest, k = 3)

# Save cluster info back into original data
QTrainTest$AnsweredCluster = clusterGroups
QTrainA = QTrainTest[1:5568,]
QTestA = QTrainTest[5569:6960,]
write.csv(QTrainA, file = "trainQuestionsAnsweredClustered.csv", row.names = FALSE)
write.csv(QTestA, file = "testQuestionAnsweredClustered.csv", row.names = FALSE)