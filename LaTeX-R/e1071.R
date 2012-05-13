library('e1071')
#Training
f = function(x) log(x)+cos(x)
x = seq(0.1,5,by=0.05)
y = f(x)+rnorm(x,sd=0.2)
svmmodel = svm(x,y)
print(svmmodel)

#Parameter Tuning
tunemodel = tune.svm(x,y,gamma=2^(-4:0),cost=2^(-2:2))
tunemodel
plot(tunemodel)



##Classification
#training & test
library(e1071)
library(rpart)
data(Glass, package = "mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex, ]
trainset <- Glass[-testindex, ]

svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -10])

rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[, -10], type = "class")

table(pred = svm.pred, true = testset[, 10])
table(pred = rpart.pred, true = testset[, 10])


##Non-linear ε-Regression
library(e1071)
library(rpart)
data(Ozone, package = "mlbench")
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex, -3])
trainset <- na.omit(Ozone[-testindex, -3])
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 1e-04)
svm.pred <- predict(svm.model, testset[, -3])
crossprod(svm.pred - testset[, 3])/length(testindex)
rpart.model <- rpart(V4 ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[, -3])
crossprod(rpart.pred - testset[, 3])/length(testindex)