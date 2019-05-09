library("HistData")
library("gbm")
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
RidMod <- gbm(response ~ .,
 data = trainingData,
 distribution = "gaussian",
 n.trees = 200,
 shrinkage = 0.5,
 interaction.depth = 3,
 bag.fraction = 0.5,
 train.fraction = 0.5,
 n.minobsinnode = 10,
 cv.folds = 3,                    
 keep.data=TRUE,              
 verbose=FALSE,               
 n.cores=1)   

predicted <- predict(RidMod, testData)
compare <- cbind(actual = testData$response, predicted)
mse[tmp] <- sum((compare[,1]-compare[,2])^2)/200
}
mse;sqrt(mean(mse));var(mse)
