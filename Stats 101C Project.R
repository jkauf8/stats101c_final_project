YelpData <- read.csv("Data_Final.csv")

YelpData.training <- YelpData[1:43076,]
YelpData.testing <- YelpData[43077:53845,]


YelpData.Numerical <- YelpData[c(4,5,6,7,11,12,13,14,15,17,18)]
Yelp.Correlation <- cor(YelpData.Numerical)

##Analyzing the Data
hist(YelpData$Star)

#Identify Correlation
library(corrplot)
corrplot(Yelp.Correlation)

##Trying QDA
library(MASS)
QDA.model1 <- qda(formula = Star ~ Bus_Ave_Star + Users_Ave_Star, data=YelpData.training)
QDA.model1.test <- predict(QDA.model1, YelpData.testing[,-4])

QDA.model1.table <- table('Reference' = YelpData.testing[,4], "Predicted"=QDA.model1.test$class)

#Assess Accuracy
(58 + 13 + 163 + 1606 + 3380)/(sum(QDA.model1.table))

##Testing a Basic Linear Model

#Identify Potentially Useful Variables
library(leaps)
summary(regsubsets(Star ~ ., data = YelpData.Numerical))

#Build Model
testmodel.1 <- lm(Star ~ Bus_Ave_Star + Users_Ave_Star + Cool + Useful, data = YelpData)
summary(testmodel.1)

#Validate

par(mfrow=c(2,2))
plot(testmodel.1)

vif(testmodel.1)

#Remove Multicollinear Variables

testmodel.2 <- lm(Star ~ Bus_Ave_Star + Users_Ave_Star, data = YelpData)

par(mfrow=c(2,2))
plot(testmodel.2)


#Test if Transformations are Needed
library(car)
summary(powerTransform(testmodel.1))

#Experiment with Interactions
testmodel.3 <- lm(Star ~ Bus_Ave_Star + Users_Ave_Star + Bus_Ave_Star:Cool + User_Fans:Cool, data = YelpData)
summary(testmodel.3)
