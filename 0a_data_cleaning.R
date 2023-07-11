
# load packages
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)

# Load in data
articles <- read_csv("data/OnlineNewsPopularity.csv") %>% 
  janitor::clean_names()

View(articles)

# skim
articles %>% 
  skim_without_charts()

# no missingness in the dataset

