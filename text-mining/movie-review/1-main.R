# Since there is only one minute difference in terms of training time
# between parallel processing and non-parallel one for this dataset.
# In addtion, parallel processing seems to break system occasinally.
# So it's not stable. So, for this project, I will not use parallel 
# processing.

library(tidyverse)
library(text2vec)
library(caret)
library(Matrix)
#library(doParallel)
#max_cores <- detectCores()
#load data
reviews <- read_csv("data/reviews_df.csv") %>%
    mutate(sentiment = as.factor(sentiment))

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
t1 <- Sys.time()
svm_control <- trainControl(method = "cv",
                            number = 5,
                            verboseIter = TRUE)
svm_grid <- expand.grid(cost = c(0.1,1)) 
#cl <- makePSOCKcluster(max_cores-2)
#registerDoParallel(cl)
fit1 <- caret::train(x = data.frame(as.matrix(dtm_train)),
                     y = train_df$sentiment,
                     method = "svmLinear2",
                     trControl = svm_control,
                     tuneGrid = svm_grid) 
t2 <- Sys.time()
