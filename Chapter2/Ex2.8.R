# ESL Exercise 2.8

### Loading the data
library(ElemStatLearn)
data(zip.train)
data(zip.test)

### Preprocessing the data
train_label <- zip.train[,1]
test_label <- zip.test[,1]

train2 <- zip.train[train_label == 2, -1]
train3 <- zip.train[train_label == 3, -1]
n2train <- nrow(train2)
n3train <- nrow(train3)
response2 <- rep(-1, n2train)
response3 <- rep(1, n3train)

train <- rbind(train2, train3)
response <- c(response2, response3)
ntrain <- nrow(train)

test2 <- zip.test[test_label == 2, -1]
test3 <- zip.test[test_label == 3, -1]
n2test <- nrow(test2)
n3test <- nrow(test3)
z2 <- rep(-1, n2test)
z3 <- rep(1, n3test)

test <- rbind(test2, test3)
z <- c(z2, z3)
ntest <- nrow(test)

### Linear Regression
model_lr <- lm(response ~ 0+train)

training.predict <- predict(model_lr, data.frame(train))
training.resid <- response - training.predict
training.error <- sum(training.resid*training.resid)/ntrain

test.predict <- test %*% coef(model_lr)
test.resid <- z - test.predict
test.error <- sum(test.resid*test.resid)/ntest

### KNN
library(class)

knn.err <- function(kay) {
  pred_training <- knn(train=train, test=train, cl=response, k=kay)
  training_error <- sum(pred_training != response)/ntrain
  pred_test <- knn(train=train, test=test, cl=response, k=kay)
  test_error <- sum(pred_test != z)/ntest
  cat('k =', kay, '\n')
  cat('training erro r =', training_error, '\n')
  cat('test error =', test_error, '\n')
}

for(k in c(1,3,5,7,15)) {
  knn.err(k)
}