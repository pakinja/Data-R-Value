
### install an load required packages
#install.packages("rpart")
#install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(RWeka)

### read and explore the data
wine <- read.csv("whitewines.csv")
str(wine)

### examine the distribution of the outcome variable
hist(wine$quality, main= "Wine Quality", col= "red")

### examine output for outliers or other potential data problems
summary(wine)

### split the data into training and testing datasets
### 75% - 25%
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

### training a model on the data

### training a regression tree model
m.rpart <- rpart(quality ~ ., data = wine_train)

### explore basic information about the tree
### for each node in the tree, the number of examples reaching
### the decision point is listed
m.rpart

### more detailed summary of tree's fit
summary(m.rpart)

### visualazing the decision tree
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

### evaluating model performance
p.rpart <- predict(m.rpart, wine_test)

### model is not correctly identifying the extreme cases
summary(p.rpart)
summary(wine_test$quality)

### correlation between actual and predicted quality
cor(p.rpart, wine_test$quality)

### Measuring performance with the mean absolute error

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

### the MAE for our predictions
MAE(p.rpart, wine_test$quality)

### the mean quality rating in the training data
mean(wine_train$quality)

### If we predicted the value 5.87 for every wine sample,
### we would have a mean absolute error of only about 0.67:
MAE(5.87, wine_test$quality)

### improving model performance?

### model tree
### model tree improves on regression trees by replacing the
### leaf nodes with regression models
### M5 algorithm (M5-prime)
m.m5p <- M5P(quality ~ ., data = wine_train)

### examine the tree and linear models
m.m5p
summary(m.m5p)

### predicted values and performance
p.m5p <- predict(m.m5p, wine_test)

summary(p.m5p)

cor(p.m5p, wine_test$quality)

MAE(wine_test$quality, p.m5p)


