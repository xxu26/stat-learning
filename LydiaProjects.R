

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
