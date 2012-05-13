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
