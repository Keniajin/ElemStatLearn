# Figure 3.5 Re-implementation

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
plotDf1 <- data.frame(cbind(xPlot, yPlot))
plotDf2 <- data.frame(cbind(xMinPlot, yMinPlot))

library(ggplot2)
ggplot(data=plotDf1, aes(x=xPlot, y=yPlot)) + geom_point() + 
  labs(title="Figure 3.5: All possible subset models for the prostate cancer example",
       x="Subset Size k", y="Residual Sum-of-Squares") +
  theme_bw() + xlim(0,8) + ylim(0,100) + 
  geom_line(data=plotDf2, aes(x=xMinPlot, y=yMinPlot, color="red")) +
  geom_point(data=plotDf2, aes(x=xMinPlot, y=yMinPlot, color="red", size=2)) +
  scale_color_discrete(guide=FALSE) + scale_size(guide=FALSE)
