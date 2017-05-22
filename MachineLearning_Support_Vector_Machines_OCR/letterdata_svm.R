### "Machine Learning in R" by Brett Lantz,
### PACKT publishing 2015
### (open source community experience destilled)


### install an load required packages
install.packages("kernlab")

library(kernlab)

### read and explore the data
letters <- read.csv("letterdata.csv")
str(letters)

### SVM require all features to be numeric and each
### feature scaled to a fairly small interval
### kernlab package perform the rescaling
### automatically

### split the data into training and testing sets
### 80% - 20%
### data is already randomized
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

### training a model on the data
### simple linear kernell function
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")
letter_classifier

### evaluating model performance
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

### improving model performance
### Gaussian RBF kernel
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                              kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf,
                                  letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

### changing the kernel function accuracy increased from
### 84% to 93%

