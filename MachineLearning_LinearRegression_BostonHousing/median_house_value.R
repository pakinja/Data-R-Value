
### install and load required packages
#install.packages("ISLR")
install.packages("car")

library(ISLR)
library(car)
### MASS library contains the Boston dataset 
library(MASS)

### explore dataset
#fix(Boston)
str(Boston)
summary(Boston)
names(Boston)

### we will seek to predict target: medv (median house value)
### using 13 predictors

### we will start with $medv as target and $lstat as predictor
### create the model
lm.fit <- lm(medv~lstat, data=Boston)

### explore the model parameters
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

### explore confidence interval for the coefficient estimates
confint(lm.fit)

### use predict() to produce confidence intervals and prediction
### intervals for the prediction of $medv for a given value of
### $lstat
predict (lm.fit , data.frame(lstat = c(5 ,10 ,15) ),
         interval = "confidence")
predict (lm.fit , data.frame(lstat = c(5 ,10 ,15) ),
         interval = "prediction")
### 95% confidence interval associated with $lstat=10 is
### (24.47, 25.63)
### 95% prediction interval is (12.828, 37.28)
### center around a predicted value of 25.05 for $medv when
### $lstat = 10

### now plot $medv and lstat along with least squares regression
### line
plot(Boston$lstat , Boston$medv, main="Boston Housing Data",
     xlab="Percent of households with low
     socioeconomic status", ylab= "Median house value")

abline(lm.fit, col = "red", lwd = 3)

### explore some diagnostic plots
par(mfrow = c(2, 2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

### leverage statistics hatvalues()
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


### multiple linear regression
### two predictors
lm.fit <- lm(medv ~ lstat + age , data = Boston)
summary(lm.fit)

### all 13 predictors
lm.fit <- lm(medv ~. , data = Boston)
summary(lm.fit)

### R^2
summary(lm.fit)$r.sq
### RSE
summary(lm.fit)$sigma

### use vif() to compute variance inflation factors
### vif is part of car library
vif(lm.fit)

### in the last regression $age has a high p-value
### run regression using all predictors but one: $age
lm.fit1 <- lm(medv ~. -age, data = Boston)
summary(lm.fit1)

### interaction terms
summary(lm(medv ~ lstat*age , data = Boston ))

### non-linear transformation of the predictors
lm.fit2 <- lm(medv ~lstat + I(lstat ^2), data = Boston)
summary(lm.fit2)

### a near cero p-value associated with the quadratic term
### leads to an improved model

### use anova() to further quantify the extent to which quadratic
### fit is superior to the linear fit
lm.fit <- lm(medv ~ lstat, data = Boston)
anova(lm.fit, lm.fit2)

### null hypothesis is that the two models fit data equally well
### alternative hypothesis is that the full model is superior
### F-statistic is 135 and p-value ~ 0 is clear evidence of
### that model containing the predictors lstat and lstat^2
### is far superior
### we saw in the above plots evidence of nonlinearity


### now use higher order polynomials with poly()
lm.fit5 = lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)

### additional polynomial terms leads to an improvement in the
### model fit

### now try a log transformation for $rm
summary (lm(medv ~ log(rm) , data = Boston ) )


