# Figure 3.8 Re-implementation

### Load and Preprocess the data
library(ElemStatLearn)
data(prostate)

training_set <- prostate[prostate$train, -10]
training_set[,-9] <- scale(training_set[,-9])

nrow <- dim(training_set)[1]
p <- dim(training_set)[2] - 1
D <- as.matrix(training_set[,1:p])
lpsa <- training_set[,p+1]
lpsaMean <- mean(lpsa)
lpsa <- lpsa - lpsaMean

### Ridge Regression
library(MASS)
num <- 10000
Lambda <- seq(from=0.0, to=10000.0, length=num)

for(i in 1:num) {
  lambda <- Lambda[i]
  M <- ginv(t(D) %*% D + lambda*diag(p)) %*% t(D)
  # ridge regression estimation
  betaHat <- M %*% as.matrix(lpsa)
  # degree of freedom
  dof <- sum(diag(D %*% M))
  if(i == 1) {
    classRes <- as.matrix(1:8)
    betaRes <- betaHat
    dofRes <- as.matrix(rep(dof,p))
  } else {
    classRes <- rbind(classRes, as.matrix(1:8))
    betaRes <- rbind(betaRes, betaHat)
    dofRes <- rbind(dofRes, as.matrix(rep(dof,p)))
  }
}

### Plot Figure 3.8
minX <- min(dofRes)
maxX <- max(dofRes) 
minY <- min(betaRes)
maxY <- max(betaRes)
plotDf <- data.frame(cbind(classRes,betaRes,dofRes))
names(plotDf) <- c("class","beta","dof")
plotDf$class <- as.factor(plotDf$class)

library(ggplot2)
ggplot(data=plotDf, aes(x=dof,y=beta,col=class)) + geom_line() +
labs(title="Figure 3.8: Profiles of ridge coefficients for the prostate cancer example", 
     x="degree of freedom", y="beta hat (ridge regression)") +
theme_bw() + scale_color_discrete(name="predictors", labels=names(training_set)) +
geom_vline(aes(xintercept=5.0, alpha=0.5), color="red", linetype="dashed", size=1) +
geom_hline(aes(yintercept=0.0, alpha=0.5), color="black", linetype="dashed", size=1)

