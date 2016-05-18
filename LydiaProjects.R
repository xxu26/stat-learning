

#####Chapter 2 Book scripts and exercises####

rm(list=ls())
set.seed(123)

#####Chapter 2
#dev.off()
#Lab 2
library(dplyr)
library(class)
library(ggplot2)
library(reshape2)
library(corrplot)
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


data <- read.csv("Auto.csv", header=T)

#7k-nearest neighbour

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

#8 melt data to long format for ggplot
college <- read.csv("College.csv", header=T, stringsAsFactors = F)
head(college)
rownames(college)= college[, 1]
fix(college)
college=college[, -1]
fix(college)
summary(college)
pairs(college[, 2:11])
college <- mutate(college, Elite=ifelse(Top10perc >50, "Yes", "No"))
par(mfrow=c(2, 2))


hist(college$Apps)
hist(college$perc.alumni, col=2)
hist(college$S.F.Ratio, col=3, breaks=10)
hist(college$Expend, breaks=100)


collegeLong<- melt(college)
ggplot(collegeLong,aes(x = value)) + 
        facet_wrap(~variable,scales = "free") +  
        geom_histogram(fill="pink",bins = 10)


corPlotbase <- cor(college[, 3:19])
par(mfrow=c(1, 1))
corrplot(corPlotbase, method="circle")
corrplot.mixed(corPlotbase)


#9
auto <- read.csv("Auto.csv", header=T, stringsAsFactors = F, na.strings = "?")#turn ? strings to NA
head(auto)
summary(auto)
sum(is.na(auto))
auto <- na.omit(auto)
sapply(auto[, 1:7], range)
sapply(auto[, 1:7], mean)
sapply(auto[, 1:7], sd)

new_auto <- auto[-(10:85), ]
new_auto[9,] == auto[9,]
new_auto[10, ]==auto[86,]
new_auto[10,]
sapply(new_auto[, 1:7], range)
sapply(new_auto[, 1:7], mean)
sapply(new_auto[, 1:7], sd)

pairs(auto[, 1:7])
cor(auto[, 1:7])

plot(auto$mpg, auto$weight)
long_auto <- melt(auto)
ggplot(long_auto,aes(x = value)) + 
        facet_wrap(~variable,scales = "free") +  
        geom_histogram(fill="blue",bins = 10)


#10
library(MASS)
Boston
summary(Boston)
dim(Boston)
names(Boston)
hist(Boston$tax)
head(Boston)
median(Boston$ptratio)
dim(subset(Boston, chas==1))
t(subset(Boston, medv==min(Boston$medv)))
summary(Boston)
Boston[399, ]
Boston[406, ]

dim(filter(Boston, rm >7))
dim(filter(Boston, rm >8))



####Chapter 3 Linear Regression########################
#Chapter 3 Linear Regression
M <- matrix(runif(120), nrow=10)
d <- dist(M)
#install.packages("igraph")
library(igraph)
net <- graph.adjacency(as.matrix(d), mode="undirected", weighted=TRUE, diag=TRUE)
plot.igraph(net)

library(MASS)
#install.packages("ISLR")
library(ISLR)
summary(lm(medv~log(rm), data=Boston))

#Applied Lab #Q8
summary(auto)
fit <-lm(mpg ~ horsepower, data=auto)
summary(fit)
4.906/mean(auto$mpg, na.rm=T)
summary(fit)$r.sq

predict(fit, data.frame(horsepower=c(98)), interval="confidence")
predict(fit, data.frame(horsepower=c(98)), interval="prediction")

plot(auto$mpg~auto$horsepower)
abline(fit)

par(mfrow=c(2, 2))
plot(fit)
par(mfrow=c(1, 1))
#Q9
pairs(auto[, 1:7])
t <- cor(subset(auto, select=-name))
corrplot(t, method="number")
fit2 <- lm(mpg ~ .-name, data=auto)
summary(fit2)
coef(fit2)["year"]
plot(fit2)
fit3 <- lm(mpg ~ cylinders * horsepower, data=auto)
summary(fit3)
fit4 <- lm(mpg~cylinders*displacement+weight, data=auto)#based on the coef
summary(fit4)
plot(fit4)
fit5 <- lm(mpg ~ log(weight) + sqrt(horsepower)+
                   acceleration+I(acceleration^2), data=auto)
summary(fit5)
plot(rstudent(fit5) ~ predict(fit5))

fit6<-lm(log(mpg)~cylinders+displacement+horsepower
         +weight+acceleration+year+origin,data=Auto)
summary(fit6)
par(mfrow=c(2,2)) 
plot(fit6)
plot(predict(fit6),rstudent(fit6))

#Q10
Carseats
summary(Carseats)
fitc <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(fitc)
# Sales = -0.05Price -0.02urbanYes + 1.2USYes + 13.4
fitc2 <- lm(Sales ~ Price + US, data=Carseats)
summary(fitc2) # Adjusted R-Squared not improve much

confint(fitc2)#obtain the 95% confidence interval
plot(predict(fitc2), rstudent(fitc2))
plot(fitc2)

#Q11
set.seed(1)
x=rnorm(100)
y=2 * x + rnorm(100)
fit11 <- lm(y ~ x + 0)
summary(fit11)
fit11.1 = lm(y~x)
summary(fit11.1)

#Q13
set.seed(1)
x=rnorm(100, mean=0, sd=1)
x=rnorm(100)
eps = rnorm(100, mean=0, sqrt(0.25))
y = -1 + 0.5*x + eps
plot(y~x)
fit13 <- lm(y~x)
summary(fit13)
abline(fit13, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
fit13.2 <- lm(y ~ x + I(x^2))
summary(fit13.2)
confint(fit13)
confint(fit13.2)

#Q14 collinearity
#a
set.seed(1)
x1=runif(100)
x2=0.5*x1 + rnorm(100)/10
y=2 + 2*x1 + 0.3*x2 + rnorm(100)
#b
cor(x1, x2)
plot(x1, x2)
#c
fit14.2 = lm(y ~ x1 + x2)
summary(fit14.2)
confint(fit14.2)
#4
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
lm.fit1 = lm(y~x1+x2)
summary(lm.fit1)
lm.fit2 = lm(y~x1)
summary(lm.fit2)
lm.fit3 = lm(y~x2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit2)
plot(lm.fit3)
plot(predict(lm.fit1), rstudent(lm.fit1))
plot(predict(lm.fit2), rstudent(lm.fit2))
plot(predict(lm.fit3), rstudent(lm.fit3))

#15
library(MASS)
summary(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)
attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
lm.chas = lm(crim~chas) 
summary(lm.chas) # no
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
lm.age = lm(crim~age)
summary(lm.age) # yes
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
lm.black = lm(crim~black)
summary(lm.black) # yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
lm.medv = lm(crim~medv)
summary(lm.medv) # yes

lm.all = lm(crim~., data=Boston)
summary(lm.all)
par(mfrow=c(1, 1))
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)


lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2, 3
# lm.chas = lm(crim~poly(chas,3)) : qualitative predictor
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3
lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # 1, 2
lm.age = lm(crim~poly(age,3))
summary(lm.age) # 1, 2, 3
lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # 1, 2, 3
lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # 1, 2
lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # 1, 2
lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # 1, 2, 3
lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1
lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # 1, 2
lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) # 1, 2, 3
