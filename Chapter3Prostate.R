# Chapter 3 Example Prostate

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

### Figure 3.5

#### k=0
mk <- lm(formula = 'lpsa ~ +1', data=training_set)
rss <- sum(mk$residuals^2)
xPlot <- c(0)
yPlot <- c(rss)

#### k=1:p
for(k in 1:p) {
  subsets <- combn(p,k)
  num <- dim(subsets)[2]
  for(i in 1:num) {
    featIndex <- subsets[,i]
    featName <- as.vector(names(training_set))[featIndex]
    form <- "lpsa ~ "
    for(j in 1:k) {
      if(j==1) {
        form = paste( form, featName[j], sep=" " )
      } else {
        form = paste( form, featName[j], sep="+" )
      }
    }
    mk <- lm(form, data=training_set)
    rss <- sum(mk$residuals^2)
    xPlot <- c(xPlot, k)
    yPlot <- c(yPlot, rss)
  }
}

#### plot figure 3.5
plot(xPlot,yPlot,xlab="Subset Size k",ylab="Residual Sum-of-Squares",ylim=c(0,100),xlim=c(0,8))

xMinPlot = xPlot[1]; yMinPlot = yPlot[1]
for ( ki in 1:p ){
  inds = xPlot==ki
  rmin = min(yPlot[inds])
  xMinPlot = c(xMinPlot,ki); yMinPlot = c(yMinPlot,rmin)
}
lines(xMinPlot,yMinPlot)
