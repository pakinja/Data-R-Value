
### "Machine Learning in R" by Brett Lantz,
### PACKT publishing 2015
### (open source community experience destilled)
### based on: Yeh IC. "Modeling of Strength of
### High Performance Concrete Using Artificial
### Neural Networks." Cement and Concrete Research
### 1998; 28:1797-1808.

### Strength of concrete example
### relationship between the ingredients used in
### concrete and the strength of finished product

### Dataset
### Compressive strength of concrete
### UCI Machine Learning Data Repository
### http://archive.ics.uci.edu/ml


### install an load required packages
#install.packages("neuralnet")

library(neuralnet)

### read and explore the data
concrete <- read.csv("concrete.csv")
str(concrete)

### Neural networks work best when the input data
### are scaled to a narrow range around zero

### normalize the dataset values
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)) )
}

### apply normalize() to the dataset columns
concrete_norm <- as.data.frame(lapply(concrete, normalize))

### confirm and compare normalization
summary(concrete_norm$strength)
summary(concrete$strength)

### split the data into training and testing sets
### 75% - 25%
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

### nnet package
### training model on the data
concrete_model <- neuralnet(strength ~ cement + slag +
              ash + water + superplastic + coarseagg + 
              fineagg + age, data = concrete_train)

### visualize the network topology
plot(concrete_model)

### there is one input node for each of the eight
### features, followed by a single hidden node and
### a single output node that predicts the concrete
### strength
### at the bottom of the figure, R reports the number
### of training steps and an error measure called the
### the sum of squared errors (SSE)

### evaluating model performance

### predictions
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result

### because this is a numeric prediction problem rather
### than a classification problem, we cannot use a confusion
### matrix to examine model accuracy
### obtain correlation between our predicted concrete strength
### and the true value
cor(predicted_strength, concrete_test$strength)

### correlation indicate a strong linear relationships between 
### two variables

### improving model performance
### increase the number of hidden nodes to five
concrete_model2 <- neuralnet(strength ~ cement + slag +
                 ash + water + superplastic + coarseagg + 
            fineagg + age, data = concrete_train, hidden = 5)

png("ann_2.png")
plot(concrete_model2)
dev.off()
### SSE has been reduced significantly

### predictions
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result

### performance
cor(predicted_strength, concrete_test$strength)

### notice that results can differs because neuralnet
### begins with random weights
### if you'd like to match results exactly, use set.seed(12345)
### before building the neural network


