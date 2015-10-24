# Table 3.1 and 3.2 Re-implementation

### Load and Preprocess the data
library(ElemStatLearn)
data(prostate)

training_set <- prostate[prostate$train, -10]
lpsa <- prostate[prostate$train, 9]
training_set$lpsa <- NULL
training_set <- scale(training_set)
means <- attr(training_set, "scaled:center")
stds <- attr(training_set, "scaled:scale")
training_set <- data.frame(training_set)
training_set$lpsa <- lpsa

test_set <- prostate[prostate$train == FALSE, -10]
lpsaTest <- prostate[prostate$train == FALSE, 9]
test_set$lpsa <- NULL
test_set <- t(apply(test_set, 1, '-', means))
test_set <- t(apply(test_set, 1, '/', stds))
test_set <- data.frame(test_set)
test_set$lpsa <- lpsaTest

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