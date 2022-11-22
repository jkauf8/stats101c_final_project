library(tidyverse)



data <- read_csv("Data_Final.csv")
data_numeric <- data[-c(1,2,3,8,9,10,16)]
results <- prcomp(data_numeric, scale = TRUE, center = TRUE)


data_ordered <- data_numeric
data_ordered$Star <- factor(data_ordered$Star, levels = c(1, 2, 3, 4, 5), ordered = TRUE)
View(data_ordered)

names(results)
summary(results)

results$rotation <- -1*results$rotation
results$rotation




var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:11), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


results$x <- -1*results$x
head(results$x)


## Ordinal Logistic Regression
library(MASS)
## make dependent variable into an ordered factor
data$Star <- factor(data$Star, levels = c(1, 2, 3, 4, 5), ordered = TRUE)

## Can make frequency tables like so
table(data$Star, data$Bus_Ave_Star)

length(data)

data_numeric$Star <- factor(data_numeric$Star, levels = c(1, 2, 3, 4, 5), ordered = TRUE)

data_train <- data_numeric[1:43076, ]
data_test <- data_numeric[-(1:43076),]



model = polr(Star ~ data_train$Useful + data_train$Cool + data_train$Funny + data_train$Bus_Ave_Star + data_train$User_Review_count + data_train$User_Useful_count + data_train$Users_Ave_Star, data = data_train, Hess = TRUE)


pred <- predict(model, data_test)
table(data_test$Star, pred)

head(pred)

train.loan_labels <- data_train$Star
test.loan_labels <- data_test$Star

library(class)

knn.26 <- knn(train=data_train, test=data_test, cl = data_train$Star, k = 232)
sum(data_test$Star == knn.26)
data_test$Star


### Here is start of KNN
set.seed(125)
dat.d <- sample(1:nrow(data_numeric), size = nrow(data_numeric)*.8, replace = FALSE)

##split up data into train and test (80/20 percent split)
train.dat <- data_numeric[dat.d, ]
test.dat <- data_numeric[-dat.d,]

train.lab <- data_numeric[, 1]
cl <- numeric(43076)
for (i in 1:nrow(train.dat)) {
  cl[i] <- train.dat$Star[i]
}

cltest <- numeric(10769)
for (i in 1:nrow(test.dat)) {
  cltest[i] <- test.dat$Star[i]
}

## KNN with k = 25 has 47 percent success.
knn.209 <- knn(train = train.dat, test = test.dat, cl, k = 25)
100 * sum(cltest == knn.209)/NROW(cltest)

table(knn.209, cltest)

## Here you can see k = 1 is actually best k with 62% and it goes
## down from there.
for (i in 1:10) {
  knn.209 <- knn(train = train.dat, test = test.dat, cl, k = i)
  cat(i, '=', 100 * sum(cltest == knn.209)/NROW(cltest), '\n')
}




