library(tidyverse)
library(text2vec)
library(caret)
library(Matrix)
#load data
reviews <- read_csv("data/reviews_df.csv")

#create train and test(75% of each positive and negative review for training, other for testing)
set.seed(123456)
train_df <- reviews %>%
    group_by(sentiment) %>%
    sample_frac(0.75) %>%
    ungroup()
test_df <- reviews[-train_df$index,]


#vocabulary-based vectorization----------------
it_train <- itoken(train_df$review,
                   preprocessor = tolower,
                   tokenizer = word_tokenizer,
                   ids = train_df$index,
                   progressbar = T)

vocab = create_vocabulary(it_train)

vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train,vectorizer)


#train with dtm without pruning and removing stop words ----------------
svm_control <- trainControl(method = "None")
svm_grid <- expand.grid(cost = c(0.1,1,1)) 
fit1 <- caret::train(x = as(dtm_train,"sparseMatrix"),
                     y = train_df$sentiment,
                     method = "svmLinear2",
                     trControl = svm_control,
                     tuneGrid = svm_grid) 
 