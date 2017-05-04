
### install and load required packages
#install.packages("psych")

library(psych)

### read and explore the data
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

### model dependent variable: $expenses
### change $charges name to $expenses
colnames(insurance)[7] <- "expenses"
summary(insurance$expenses)
hist(insurance$expenses, main = "Insurance Expenses", col = "red",
     xlab = "Expenses (USD)")

### explore $region
table(insurance$region)

### exoploring relationships among features
### correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

### visualizing relationships among features
### scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])

### scatterplots, distributions and correlation
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

### training a model on the data
ins_model <- lm(expenses ~ age + children + bmi + sex +
                  smoker + region, data = insurance)
### this do the same
#ins_model <- lm(expenses ~ ., data = insurance)

### explore model parameters
ins_model

### evaluating model performance
summary(ins_model)

### improving model performance

### adding non-linear relationships
### adding second order term on $age
insurance$age2 <- insurance$age^2

### converting a numeric variable to a binary indicator
### $bmi feature only have impact above some value
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

### putting it all together
### improved regression model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)



