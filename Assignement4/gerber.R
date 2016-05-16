# Part1

gerber = read.csv("gerber.csv")
mean(gerber$voting)
mean(gerber[gerber$hawthorne == 1,]$voting)
mean(gerber[gerber$civicduty== 1,]$voting)
mean(gerber[gerber$self == 1,]$voting)
mean(gerber[gerber$neighbors == 1,]$voting)

model = glm(voting ~ hawthorne + civicduty + self + neighbors, data = gerber)
summary(model)

predictions = predict(model)
table(gerber$voting, predictions > 0.3)
accuracy = (134513 + 51966) / nrow(gerber)
table(gerber$voting, predictions > 0.5)
235388 / nrow(gerber)
