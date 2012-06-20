library("kernlab")
data("iris")
irismodel <- ksvm(Species ~ ., data=iris,
                  type="C-bsvc",kernel="rbfdot",
                  kpar=list(sigma=0.1),C=10,
                  prob.model=TRUE)
irismodel

predict(irismodel, iris[c(3, 10, 56, 68,107, 120), -5], type = "probabilities")

predict(irismodel, iris[c(3, 10, 56, 68,107, 120), -5], type = "decision")



###
k <- function(x, y){
    (sum(x * y) + 1) * exp(0.001 * sum((x - y)^2))
}

data("promotergene")
gene <- ksvm(Class ~ ., data = promotergene,
            kernel = k, C = 10, cross = 5)
gene

x <- rbind(matrix(rnorm(120), , 2), matrix(rnorm(120,mean = 3), , 2))
y <- matrix(c(rep(1, 60), rep(-1, 60)))
svp <- ksvm(x, y, type = "C-svc", kernel = "rbfdot",kpar = list(sigma = 2))
plot(svp)

#####
library("e1071")
model <- svm(Species ~ ., data = iris,method = "C-classification", kernel = "radial",cost = 10, gamma = 0.1)
summary(model)

plot(model, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))

(pred <- predict(model, head(iris), decision.values = TRUE))

attr(pred, "decision.values")

data("spam")
tobj <- tune.svm(type ~ ., data = spam_train[1:300,], gamma = 10^(-6:-3), cost = 10^(1:2))
summary(tobj)
plot(tobj, transform.x = log10, xlab = expression(log[10](gamma)), ylab = "C")
bestGamma <- tobj$best.parameters[[1]]
bestC <- tobj$best.parameters[[2]]
model <- svm(type ~ ., data = spam_train,cost = bestC, gamma = bestGamma, cross = 10)
summary(model)

#####
library("klaR")
data("B3")
Bmod <- svmlight(PHASEN ~ ., data = B3 , svm.options = "-c 10 -t 2 -g 0.1 -v 0")
predict(Bmod, B3[c(4, 9, 30, 60, 80, 120),-1])

#####
library("svmpath")
data("svmpath")
attach(balanced.overlap)
svmpm <- svmpath(x, y, kernel.function = radial.kernel,param.kernel = 0.1)
predict(svmpm, x, lambda = 0.1)
predict(svmpm, lambda = 0.2, type = "alpha")




