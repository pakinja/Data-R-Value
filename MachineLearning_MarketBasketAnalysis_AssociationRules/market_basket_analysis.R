
### install and load required packages
install.packages("arules")

library(arules)

### read and explore the data
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)

### density value 0.0260888 (26%) refers to the proportion
### of nonzero matrix cells

### since 2513/9835 = 0.2555, we can determine that whole
### milk appeared in 25.6% of the transactions

### a total of 2159 transactions contained only a single item
### 4.409 items in mean per transaction

### examining transaction data
inspect(groceries[1:5])

itemFrequency(groceries[, 1:3])

### visualizing item support = 10%
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

### visualizing the transaction data, sparse matrix
image(groceries[1:5])
image(sample(groceries, 100))

### training a model on the data
### default settings support = 0.1 and confidence = 0.8
apriori(groceries)

### adjusting parameters
groceryrules <- apriori(groceries, parameter = list(support =
            0.006, confidence = 0.25, minlen = 2))
### set of 463 rules

### evaluating model performance
summary(groceryrules)

### inspect somre rules
inspect(groceryrules[1:3])

### improving model performance

### sorting the set of association rules
inspect(sort(groceryrules, by = "lift")[1:5])

### lift = 3.96 implies that people who buys herbs are nearly
### four times more likely to buy root vegetables than typical
### costumer

### taking subsets of association rules
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

### there are four rules involving berries, two of which seem
### to be interesting enough to be called actionable

### saving association rules to a file or dataframe
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

