### "An Introduction to Statistical Learning.
### With applications in R" by Gareth James,
### Daniela Witten, Trevor Hastie and Robert Tibshirani.
### Springer 2015.


### install and load required packages
library(ISLR)
library(psych)
library(MASS)

### perform linear discriminant analysis LDA on the stock
### market data
train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train, ]
Direction.2005 <- Smarket$Direction[!train]

lda.fit <- lda(Direction ~ Lag1 + Lag2 , data = Smarket , subset = train )
lda.fit

### LDA indicates that 49.2% of training observations
### correspond to days during wich the market went down

### group means suggest that there is a tendency for the
### previous 2 days returns to be negative on days when the
### market increases, and a tendency for the previous days
### returns to be positive on days when the market declines

### coefficients of linear discriminants output provides the
### linear combination of Lag1 and Lag2 that are used to form
### the LDA decision rule

### if (−0.642 * Lag1 − 0.514 * Lag2) is large, then the LDA
### classifier will predict a market increase, and if it is
### small, then the LDA classifier will predict a market decline

### plot() function produces plots of the linear discriminants,
### obtained by computing (−0.642 * Lag1 − 0.514 * Lag2) for
### each of the training observations
plot (lda.fit)

### predictions
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

### the LDA and logistic regression predictions are almost identical

### apply a 50% threshold to the posterior probabilities allows
### us to recreate the predictions in lda.pred$class
sum(lda.pred$posterior [ ,1] >= 0.5)
sum(lda.pred$posterior [ ,1] < 0.5)

### posterior probability output by the model corresponds to
### the probability that the market will decrease
lda.pred$posterior[1:20 ,1]
lda.class[1:20]

### use a posterior probability threshold other than 50 % in order
### to make predictions

### suppose that we wish to predict a market decrease only if we
### are very certain that the market will indeed decrease on that
### day-say, if the posterior probability is at least 90%
sum(lda.pred$posterior[ ,1] > 0.9)

### No days in 2005 meet that threshold! In fact, the greatest
### posterior probability of decrease in all of 2005 was 52.02%
