#### wRANGLE ####
# At first we load some libraries we are going to use:
library(tidyverse)
library(caret)

# Divide the edx data set into train and test sets:
set.seed(1, sample.kind = 'Rounding')
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# Remove the sets we won't use
rm(test_index, temp, removed)

# Establish the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#### FIRST MODEL (MOVIE/USER/GENRE VARIATIONS) ####
# Calculate the average rating
mu <- mean(train_set$rating)

# Calculating the Movie and user effects
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

RMSE(predicted_ratings, test_set$rating)
#Our first RMSE, still needs improoving

#### SECOND MODEL (MOVIE/USER/GENRE VARIATIONS - REGULARIZED) ####
# Adding regularization
lambdas <- seq(0, 10, 0.25)

#tuning the lambda
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

#best tuned lambda
lambda <- lambdas[which.min(rmses)]

#final regularized model RMSE
min(rmses)

#### FINAL MODEL (MOVIE/USER/GENRE VARIATIONS - REGULARIZED[EDX AND VALIDATION]) ####
#Train the algorithm in the hole edx dataset
#using the same logic and the lambda that was tuned on the second model 
# Average of the whole dataset
mu_f <- mean(edx$rating)

# Movie Bias
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# User Bias
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_f)/(n()+lambda))

#Genre Bias
b_g <- edx %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu_f)/(n()+lambda))

# Final Prediction
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu_f + b_i + b_u + b_g) %>%
  .$pred

#Final RMSE
RMSE(predicted_ratings, validation$rating)
