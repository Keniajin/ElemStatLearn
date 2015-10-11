### Figure 3.5 Re-implementation

### Load and Preprocess the data
library(ElemStatLearn)
data(prostate)

training_set <- prostate[prostate$train, -10]
training_set[,-9] <- scale(training_set[,-9])
p <- dim(training_set)[2] - 1

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