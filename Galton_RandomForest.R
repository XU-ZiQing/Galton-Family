library("HistData")
library("randomForest")
set.seed(1)
attach(GaltonFamilies)
str(GaltonFamilies)
inputData <- data.frame(GaltonFamilies)
colnames(inputData)[8]<-"response"
colnames(inputData)[6]<-"gender"
inputData[ ,6] <- as.numeric(inputData[,7]=="male")
inputData <- inputData[,-c(1,7)]

mse=1:50
for (tmp in 1:50){
trainingIndex <- sample(1:nrow(inputData),nrow(inputData)-200)
trainingData <- inputData[trainingIndex, ]
testData <- inputData[-trainingIndex, ]
RidMod <- randomForest(response ~ ., data = trainingData, importance = TRUE, proximmity = FALSE, ntree = 200)
predicted <- predict(RidMod, testData)
compare <- cbind(actual = testData$response, predicted)
mse[tmp] <- mean((compare[,1]-compare[,2])^2)
}
mse;sqrt(mean(mse));var(mse)
