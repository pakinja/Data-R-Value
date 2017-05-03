
### install and load required packages
#install.packages("RWeka")

library(RWeka)

### read and explore the data
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

### drop $veil_type column (does not provide)
### useful information
mushrooms$veil_type <- NULL

### look at the distribution of the muschroom type
### class variable
table(mushrooms$type)

### data transformation

#Below function transforms the $type column
transformType <- function(key){
  switch (as.character(key),
          'p' = 'poisonous',
          'e' = 'edible'
  )
}

#Below function transforms the $odor column
transformOdor <- function(key){
  switch (as.character(key),
          'a' = 'almond',
          'l' = 'anise',
          'c' = 'creosote',
          'y' = 'fishy',
          'f' = 'foul',
          'm' = 'musty',
          'n' = 'none',
          'p' = 'pungent',
          's' = 'spicy'
  )
}

### apply transformations
mushrooms$type <- sapply(mushrooms$type, transformType)
mushrooms$odor <- sapply(mushrooms$odor, transformOdor)

mushrooms$type <- as.factor(mushrooms$type)
mushrooms$odor <- as.factor(mushrooms$odor)
### training a model on the data

### use the 1R implementation in the RWeka package
### called OneR()

### consider all possible features to predict $type
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

### evaluating model performance
summary(mushroom_1R)

### improving model performance
### JRip() Java-based implementation of the Ripper
### rule learning algorithm
mushroom_JRip <- JRip(type ~ ., data = mushrooms)

### explore the rules
mushroom_JRip


