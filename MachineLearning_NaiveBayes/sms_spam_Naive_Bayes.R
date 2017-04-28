
### install required packages

install.packages("tm")
#install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("e1071")
install.packages("RColorBrewer")
install.packages("gmodels")

library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(e1071)
library(gmodels)

### read the data
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

### transform type into factor (ham/spam)
sms_raw$type <- factor(sms_raw$type)

### see frequencies
table(sms_raw$type)

### create volatile (stored in memory) corpus 
### Vcorpus create a complex list, we can use list manipulation
### commands to manage it
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

### inspect the two first elements 
inspect(sms_corpus[1:2])

### see the first sms (as text)
as.character(sms_corpus[[1]])

### to view multiple documents
lapply(sms_corpus[1:2], as.character)

### cleaning the text in documents (corpus)

### tm_map to map all over the corpus
### content_transformer to access the corpus
### tolower to lowercase all strings
sms_corpus_clean <- tm_map(sms_corpus,
                           content_transformer(tolower))

### check and compare the result of the cleaning
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

### remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

### remove "stop words"
### see the "stop words" list
stopwords()

sms_corpus_clean <- tm_map(sms_corpus_clean,
                           removeWords, stopwords())
### remove punctuation

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

### stemming (transform words into it's root form)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

### romove additional whitespaces
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

### check and compare result of cleaning
### OJO
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

### tokenization
### document term matrix (DTM)
### rows are sms and columns are words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

### data preparation (training and test sets)
### 75% train 25% test
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

### labels for train and test sets
### feature to be classified
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

### confirm that the subsets are representative of the 
### complete set of SMS data
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

### wordcloud
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

### creating indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

### filter DTM by frequent terms
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

### change DTM frequency to factor (categorical)
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_counts)

### training a model on the data
### alternative package for Naives-Bayes ("klaR")
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

### evaluating model performance
### make predictions on test set
sms_test_pred <- predict(sms_classifier, sms_test)

### compare predictions (classifications) with the
### true values
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

### improving model performance
### set Laplace estimator to 1
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


