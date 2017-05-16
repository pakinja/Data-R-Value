### "An Introduction to Statistical Learning.
### With applications in R" by Gareth James,
### Daniela Witten, Trevor Hastie and Robert Tibshirani.
### Springer 2015.


### install and load required packages
library(ISLR)
library(psych)
library(MASS)
library(class)

### split the dataset into train and test sets
train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train, ]
Direction.2005 <- Smarket$Direction[!train]

### Quadratic Discriminant Analysis

### perform quadratic discriminant analysis QDA on the stock
### market data
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

### the output does not contain the coefficients of the linear discriminants,
### because the QDA classifier involves a quadratic, rather than a linear,
### function of the predictors
qda.fit

### predictions
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

### QDA predictions are accurate almost 60 % of the time,
### even though the 2005 data was not used to fit the model
### quite impressive for stock market data, which is known to
### be quite hard to model accurately

### K-Nearest Neighbors

### split the dataset into train and test sets
train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
train.Direction <- Smarket$Direction[train]

### use knn() function can be used to predict the market’s movement
### for the dates in 2005
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)                   
(83+43)/252                   

### the results using K = 1 are not very good, since only 50 % of the
### observations are correctly predicted

### try k=3
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

### the results have improved slightly. But increasing K further turns out
### to provide no further improvements. It appears that for this data, QDA
### provides the best results of the methods that we have examined so far
