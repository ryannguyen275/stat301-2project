
# load packages
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
set.seed(13)

# Load in data
articles <- read_csv("data/OnlineNewsPopularity.csv") %>% 
  janitor::clean_names() %>% 
  select(-c(url, timedelta))

View(articles)

# Look at Distribution of Outcome Variable, Shares
articles %>% 
  summarise(mean = mean(shares),
            min = min(shares),
            q1 = quantile(shares, probs = 0.25),
            median = median(shares),
            q3 = quantile(shares, probs = 0.75),
            max = max(shares),
            sd = sd(shares))

## Super right-skewed, need to make symmetric
ggplot(articles, aes(x = shares)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Shares")

## slightly better, still very right-skewed
ggplot(articles, aes(x = sqrt(shares))) +
  geom_histogram()

## Much more symmetrical
ggplot(articles, aes(x = log10(shares))) +
  geom_histogram(bins = 50)+
  labs(title = "Distribution of Shares (Log-Transformed")

# Need to log transform `shares` on entire data set
articles <- articles %>% 
  mutate(log_shares = log10(shares))  %>% 
  # adding column that combines day of the week, and data channel, into one
  mutate(day_is_monday = ifelse(weekday_is_monday == 1, "monday", ""),
         day_is_tuesday = ifelse(weekday_is_tuesday == 1, "tuesday", ""),
         day_is_wednesday = ifelse(weekday_is_wednesday == 1, "wednesday", ""),
         day_is_thursday = ifelse(weekday_is_thursday == 1, "thursday", ""),
         day_is_friday = ifelse(weekday_is_friday == 1, "friday", ""),
         day_is_saturday = ifelse(weekday_is_saturday == 1, "saturday", ""),
         day_is_sunday = ifelse(weekday_is_sunday == 1, "sunday", "")) %>% 
  unite(col = "day_of_week", starts_with(c("day_is_")), sep = "") %>% 
  mutate(channel_is_lifestyle = ifelse(data_channel_is_lifestyle == 1, "lifestyle", ""),
         channel_is_entertainment = ifelse(data_channel_is_entertainment == 1, "entertainment", ""),
         channel_is_bus = ifelse(data_channel_is_bus == 1, "business", ""),
         channel_is_socmed = ifelse(data_channel_is_socmed == 1, "social media", ""),
         channel_is_tech = ifelse(data_channel_is_tech == 1, "tech", ""),
         channel_is_world = ifelse(data_channel_is_world == 1, "world", "")) %>% 
  unite(col = "data_channel", starts_with(c("channel_is_")), sep = "")

write_csv(articles, "data/articles.csv")

articles <- read_csv("data/articles.csv")

# SPLITTING THE DATA 
articles_split <- initial_split(articles, prop = 0.8, strata = log_shares)

articles_train <- training(articles_split)

articles_test <- testing(articles_split)

articles_folds <- vfold_cv(articles_train, v = 5, repeats = 3, strata = log_shares)

save(articles_split, articles_train, articles_test, articles_folds, file = "data/articles_split.rda")

load("data/articles_split.rda")

dim(articles_train)

dim(articles_test)

# At first glance, data channel has no missingness, but after looking at all variables,
# we see that 4,917 are missing data channel 
articles_train %>% 
  filter(data_channel_is_lifestyle == 0,
         data_channel_is_entertainment == 0,
         data_channel_is_bus == 0,
         data_channel_is_socmed == 0,
         data_channel_is_tech == 0,
         data_channel_is_world == 0,) %>% 
  select(starts_with(c("data_channel_is"))) %>% 
  View()

# data channel with most shares
articles_train %>% 
  group_by(data_channel) %>%
  summarise(sum = sum(log_shares),
            count = n()) %>% 
  mutate(avg_shares = sum/count) %>% 
  filter(data_channel != "") %>% 
  ggplot(aes(x = data_channel, y = avg_shares)) +
           geom_col() +
  labs(title = "Average Shares For Each Data Channel", x = "Data Channel", y = "Average Log_Shares")


# all days of the week are accounted for
articles_train %>% 
  filter(weekday_is_monday == 0,
         weekday_is_tuesday == 0,
         weekday_is_wednesday == 0,
         weekday_is_thursday == 0,
         weekday_is_friday == 0,
         weekday_is_saturday == 0,
         weekday_is_sunday == 0) %>% 
  select(starts_with(c("weekday_is")))

articles_train %>% 
  group_by(day_of_week) %>%
  summarise(sum = sum(log_shares),
            count = n()) %>% 
  mutate(avg_shares = sum/count) %>% 
  ggplot(aes(x = day_of_week, y = avg_shares)) +
  geom_col()

# relationship b/t number of images in article + shares
articles_train %>%
  ggplot(aes(x = num_imgs, y = log_shares)) + 
  geom_point(alpha = 0.2)  +
  geom_smooth()

# relationship b/t number of videos in article + shares
articles_train %>%
  ggplot(aes(x = num_videos, y = log_shares)) + 
  geom_point(alpha = 0.2) +
  geom_smooth()

# amount of media, as videos increase, images decrease
articles_train %>%
  ggplot(aes(x = num_imgs, y = num_videos)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue")

# looking at subjectivity
articles_train %>% 
  ggplot(aes(x = global_subjectivity, y = log_shares)) + 
  geom_point(alpha = 0.2)

articles_train %>% 
ggplot(aes(x = title_subjectivity, y = log_shares)) + 
  geom_point(alpha = 0.2)

# relationship b/t number of links in article + shares
articles_train %>%
  ggplot(aes(x = num_hrefs, y = log_shares)) +
  geom_point(alpha = 0.2)

# looking at positive vs. negative words
articles_train %>%
  ggplot(aes(x = rate_positive_words, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x = rate_negative_words, y = log_shares)) +
  geom_point(alpha = 0.2)

# average article length
articles_train %>%
  ggplot(aes(x = average_token_length, y = log_shares)) +
  geom_point(alpha = 0.2)

# average article length
articles_train %>%
  ggplot(aes(x = num_keywords, y = log_shares)) +
  geom_col() + 
  labs(x = "number of keywords", y = "log10(shares)" ,title = "Log_Shares vs. Number of Keywords")

# average shares of referenced articles
articles_train %>%
  ggplot(aes(x = self_reference_avg_shares, y = log_shares)) +
  geom_point(alpha = 0.2)

# looking at positive vs. negative polarity
articles_train %>%
  ggplot(aes(x =  avg_positive_polarity, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x =  avg_negative_polarity, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x =  avg_negative_polarity, y = avg_positive_polarity)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x = title_sentiment_polarity, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x =  abs_title_sentiment_polarity, y = log_shares)) +
  geom_point(alpha = 0.2)

# LDA
articles_train %>%
  ggplot(aes(x =  lda_00, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x =  lda_01, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x =  lda_02, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x =  lda_03, y = log_shares)) +
  geom_point(alpha = 0.2)

# n tokens
articles_train %>%
  ggplot(aes(x =  n_tokens_content, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  filter(n_non_stop_unique_tokens < 100) %>% 
  ggplot(aes(x =  n_non_stop_unique_tokens, y = log_shares)) +
  geom_point(alpha = 0.2)

articles_train %>%
  ggplot(aes(x =  num_keywords, y = log_shares)) +
  geom_col()

articles_train %>%
  ggplot(aes(x =  kw_avg_avg, y = log_shares)) +
  geom_point(alpha = 0.2)




