

#####Chapter 2 Book scripts and exercises####

rm(list=ls())
set.seed(123)
#dev.off()
#Lab 2
x=1:10
x
x=seq(-pi,pi,length=50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)


data <- read.csv("../Auto.csv", header=T)
library(dplyr)

#Q7 k-nearest neighbour
library(class)
testData <- read.csv("kmeans.csv", header=T)
train <- testData[1:6, 1:3]
test <- testData[7, 1:3]
train_label <- testData[1:6, 4]
test_label <- testData[7, 4]
pred <- knn(train, test, cl=train_label, k=1)
pred
pred2 <- knn(train, test, cl=train_label, k=2)
pred2
pred3 <- knn(train, test, cl=train_label, k=3)
pred3
