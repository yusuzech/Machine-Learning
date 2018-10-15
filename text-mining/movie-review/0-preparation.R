library(tidyverse)
#load data into a dataframe and save to csv
pos_review_path = list.files("data/movie_reviews/pos/",full.names = T)
neg_review_path = list.files("data/movie_reviews/pos/",full.names = T)

review_df <- tibble(sentiment = rep("positive",length(pos_review_path)),
                    review = map_chr(pos_review_path,~ read_file(.x))) %>% 
    bind_rows(tibble(sentiment = rep("negative",length(neg_review_path)),
                     review = map_chr(neg_review_path,~ read_file(.x)))) %>%
    mutate(index = row_number()) %>%
    select(index,sentiment,review)

write_csv(review_df,"data/reviews_df.csv")
 