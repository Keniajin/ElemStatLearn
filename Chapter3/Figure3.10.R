# Figure 3.10 Re-implementation

### Load and Preprocess the data
library(ElemStatLearn)
data(prostate)

training_set <- prostate[prostate$train, -10]
lpsa <- prostate[prostate$train, 9]
lpsaMean <- mean(lpsa)
lpsa <- lpsa - lpsaMean
training_set$lpsa <- NULL
training_set <- scale(training_set)
means <- attr(training_set, "scaled:center")
stds <- attr(training_set, "scaled:scale")
training_set <- data.frame(training_set)
training_set$lpsa <- lpsa

nrow <- dim(training_set)[1]
p <- dim(training_set)[2] - 1

### Lasso
library(glmnet)

lm.fit <- lm(lpsa~., data=training_set)
coef_sum <- sum(abs(lm.fit$coefficients))

lasso.fit <- glmnet(as.matrix(training_set[,1:p]), training_set[,p+1], 
              family="gaussian", alpha=1)
lambda <- lasso.fit$lambda
beta <- lasso.fit$beta
s <- apply(abs(beta), 2, sum)/coef_sum
num <- length(lambda)

for(i in 1:num) {
  if(i == 1) {
    ShrinkageFactor <- as.matrix(rep(s[i], p))
    classRes <- as.matrix(1:p)
    betaRes <- as.matrix(beta[,i])
  } else {
    ShrinkageFactor <- rbind(ShrinkageFactor, as.matrix(rep(s[i],p)))
    classRes <- rbind(classRes, as.matrix(1:p))
    betaRes <- rbind(betaRes, as.matrix(beta[,i]))
  }
}

### Plot Figure 3.7
plotDf <- data.frame(cbind(classRes,betaRes,ShrinkageFactor))
names(plotDf) <- c("class","beta","s")
plotDf$class <- as.factor(plotDf$class)

library(ggplot2)
ggplot(data=plotDf, aes(x=s,y=beta,col=class)) + geom_line() +
  labs(title="Figure 3.10: Profiles of lasso coefficients for the prostate cancer example", 
       x="Shrinkage Factor s", y="Coefficients (Lasso)") +
  theme_bw() + scale_color_discrete(name="predictors", labels=names(training_set)) +
  geom_vline(aes(xintercept=0.37, alpha=0.5), color="red", linetype="dashed", size=1)
