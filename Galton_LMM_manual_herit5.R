# import data
library("HistData")
attach(GaltonFamilies)
str(GaltonFamilies)
inputData <- data.frame(GaltonFamilies)

# rename the columns
colnames(inputData)[8]<-"response"
colnames(inputData)[6]<-"gender"
inputData[ ,6] <- as.numeric(inputData[,7]=="male")

# fixed the random seed
set.seed(1)

inputData <- inputData[,-7]
inputData[ ,4] <- inputData[,5]*0+1

# coefficient matrix 
beta.opt <- matrix(0,5,50)
delta.opt <- c()
mse <- c()

# try our method for 50 times
for (temp in 1:50){

# randomly choose 200 identities from the data set as testing set and the remained 734 as training set
trainIndex <- sample(1:nrow(inputData),nrow(inputData)-200)
trainData <- inputData[trainIndex, ]
testData <- inputData[-trainIndex, ]

# compute the genetic relationship matrix
train.number <- length(trainData$family)
family.unique <- unique(inputData$family)
family.number <- length(family.unique)
n <- train.number
K <- matrix(0, nrow=train.number, ncol=train.number)
for (i in 1:n){
 K[i, which(trainData$family==trainData$family[i])] <- 0.5
 K[i,i] <- 1;
}

eig <- eigen(K)
s <- eig$values
U <- eig$vectors

y <- trainData$response
X <- cbind(trainData$father,trainData$mother,trainData$midparentHeight,trainData$children,trainData$gender)
UTy <- t(U)%*%y
UTX <- t(U)%*%X

UXXU<-array(0,dim=c(5,5,train.number))
for (i in 1:train.number){
UXXU[,,i] <- crossprod(t(UTX[i,]), t(UTX[i,])) #so ugly
}

LL <- c()
LL.opt <- -2000
for (delt in 1:50)
{
 delta = exp(delt*0.1)
 beta.half <- matrix(0,5,5)
 for (i in 1:n){
  beta.half <- beta.half + UXXU[,,i]/(delta+s[i])
 }
 betay <- 0
 for (i in 1:n){
  betay <- betay + t(t(UTX[i,]))*UTy[i]/(s[i]+delta)
 }
 beta <- solve(beta.half)%*%betay
 sigmau2 <- 0
 for (i in 1:train.number){
 sigmau2 <- sigmau2+(UTy[i]-UTX[i,]%*%beta)^2/(n*(s[i]+delta))
 } 
 logS <- sum(log(s+delta))
 LL[delt] <- -0.5*(n*log(2*pi)+logS+n+n*log(sigmau2))
}
delt = which(LL==max(LL))[1]
delta = exp(delt*0.1)
delta.opt[temp] = delta
beta.half <- matrix(0,5,5)
 for (i in 1:n){
  beta.half <- beta.half + UXXU[,,i]/(delta+s[i])
 }
 betay <- 0
 for (i in 1:n){
  betay <- betay + t(t(UTX[i,]))*UTy[i]/(s[i]+delta)
 }
 beta <- solve(beta.half)%*%betay
 sigmau2 <- 0
 for (i in 1:train.number){
 sigmau2 <- sigmau2+(UTy[i]-UTX[i,]%*%beta)^2/(n*(s[i]+delta))
 } 
beta.opt[,temp] <- beta
y2 <- testData$response
X2 <- cbind(testData$father,testData$mother,testData$midparentHeight,testData$children,testData$gender)
y2.hat <- X2 %*% beta
mse[temp] <- mean((y2-y2.hat)^2)
}
mse;sqrt(mean(mse));
