# Table 3.1 and 3.2 Re-implementation

### Load and Preprocess the data
library(ElemStatLearn)
data(prostate)

training_set <- prostate[prostate$train, -10]
test_set <- prostate[prostate$train == FALSE, -10]
training_set[,-9] <- scale(training_set[,-9])
test_set[,-9] <- scale(test_set[,-9])

### Table 3.1
D <- training_set[,-9]
print(cov(D),digits=3) # Table 3.1

### Table 3.2
nrow <- dim(training_set)[1]
p <- dim(training_set)[2] - 1
Dp = cbind(matrix(1, nrow, 1), as.matrix(D))
lpsa = training_set[,9]

library(MASS)
betaHat <- ginv(t(Dp) %*% Dp) %*% t(Dp) %*% as.matrix(lpsa)
yhat <- Dp %*% betaHat
sigmaHat <- sum((lpsa - yhat)^2)/(nrow - p - 1)
covarBetaHat <- sigmaHat * ginv(t(Dp) %*% Dp)
stdBetaHat <- sqrt(diag(covarBetaHat))
z <- betaHat/stdBetaHat
print('first column: beta estimates')
print(betaHat,digits=2)
print('second column: beta standard errors')
print( as.matrix(stdBetaHat), digits=2 )
print('third column: beta z-scores')
print( z, digits=2 )

lr <- lm(lpsa ~ ., data=training_set)
print('using the R lm function')
print( summary(lr), digits=2 )

### Mean Squared Error & Standard Error
predict_lr <- predict(lr, test_set)
MSE <- mean((predict_lr - test_set$lpsa)^2)
print(MSE)
ntest <- length(predict_lr)
SE <- sqrt(var((predict_lr - test_set$lpsa)^2)/ntest)
print(SE)