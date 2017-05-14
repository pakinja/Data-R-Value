### "An Introduction to Statistical Learning.
### With applications in R" by Gareth James,
### Daniela Witten, Trevor Hastie and Robert Tibshirani.
### Springer 2015.


### install and load required packages
library(ISLR)
library(psych)

### explore the dataset
names(Smarket)
dim(Smarket)
summary(Smarket)

### correlation matrix
cor(Smarket[,-9])

### correlations between th lag variables and today
### returns are close to zero
### the only substantial correlation is between $Year
### and $Volume
plot(Smarket$Volume, main= "Stock Market Data",
     ylab = "Volume")

### scatterplots, distributions and correlations
pairs.panels(Smarket)

### fit a logistic regression model to predict $Direction
### using $Lag1 through $Lag5 and $Volume
### glm(): generalized linear model function
### family=binomial => logistic regression
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = binomial)
summary(glm.fit)

### the smallest p_value is associated with Lag1
### the negative coefficient for this predictor suggests
### that if the market had a positive return yesterday,
### then it is less likely to go up today
### at a value of 0.15, the p-value is still relatively large,
### and so there is no clear evidence of a real association
### between $Lag1 and $Direction

### explore fitted model coefficients
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[ ,4]

### predict the probability that the market will go up,
### given values of the predictors
glm.probs <- predict(glm.fit, type="response")
glm.probs[1:10]
contrasts(Smarket$Direction)

### these values correspond to the probability of the market
### going up, rather than down, because the contrasts()
### function indicates that R has created a dummy variable with
### a 1 for Up

### create a vector of class predictions based on whether the
### predicted probability of a market increase is greater than
### or less than 0.5
glm.pred <- rep ("Down", 1250)
glm.pred[glm.probs > .5] <- "Up"

### confusion matrix in order to determine how many observations
### were correctly or incorrectly classified
table(glm.pred, Smarket$Direction)
mean(glm.pred == Smarket$Direction)

### model correctly predicted that the market would go up on 507
### days and that it would go down on 145 days, for a total of
### 507 + 145 = 652 correct predictions
### ogistic regression correctly predicted the movement of the
### market 52.2 % of the time

### to better assess the accuracy of the logistic regression model
### in this setting, we can fit the model using part of the data,
### and then examine how well it predicts the held out data
train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)

Direction.2005 <- Smarket$Direction[!train]

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")

### compute the predictions for 2005 and compare them to the actual
### movements of the market over that time period
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

### not generally expect to be able to use previous days returns to
### predict future market performance

### refit the logistic regression using just Lag1 and Lag2 , which
### seemed to have the highest predictive power in the original logistic
### regression model
glm.fit <- glm(Direction ~ Lag1 + Lag2 , data = Smarket,
               family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005 , type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

### results appear to be a little better: 56%

### if we want to predict the returns associated with particular
### values of $Lag1 and $Lag2
predict(glm.fit, newdata = data.frame(Lag1 = c (1.2 ,1.5),
           Lag2 = c(1.1, -0.8)) , type = "response")




