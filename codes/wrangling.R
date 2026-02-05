# Packages
library(tidyverse)
library(lubridate)
library(corrplot)
library(vars)
library(MSwM)
library(zoo)
library(xtable)

setwd("~/Documents/GitHub/PhDEcon107/codes/")
sentiment_scored_aggregated_full <- read_csv("~/GitHub/PhDEcon107/sentiment_scored_aggregated_full.csv")
sentiment_scored_aggregated <- read.csv("~/Documents/GitHub/PhDEcon107/data/sentiment_scored_aggregated.csv")

missing_2021_dates <- sentiment_scored_aggregated %>%
  filter(format(DATE, "%Y") == "2021") %>%
  anti_join(
    sentiment_scored_aggregated_full,
    by = "DATE"
  )

sentiment_scored_aggregated_full_teste <- bind_rows(
  sentiment_scored_aggregated_full,
  missing_2021_dates
) %>%
  arrange(DATE)


icc <- read.csv2("~/Documents/GitHub/PhDEcon107/data/icc.csv")
names(icc)<- c('DATE', 'icc')

icc <- icc %>%
  mutate(DATE = floor_date(as.Date(DATE), unit = "month"))

sentiment_scored_aggregated$DATE <- as.Date(sentiment_scored_aggregated$DATE)

# wrangling
data <- sentiment_scored_aggregated_full_teste %>%
  mutate(DATE = floor_date(DATE, unit = "month")) %>%
  group_by(DATE) %>%
  summarise(
    weighted_sentiment_finbert = sum(sentiment_finbert * LEN, na.rm = TRUE) / sum(LEN, na.rm = TRUE),
    weighted_sentiment_yiyanghkust = sum(sentiment_yiyanghkust * LEN, na.rm = TRUE) / sum(LEN, na.rm = TRUE),
    total_length = sum(LEN, na.rm = TRUE),
    n_docs = n()
  ) %>%
  ungroup() %>%
  mutate(
    norm_finbert = (weighted_sentiment_finbert - min(weighted_sentiment_finbert)) /
      (max(weighted_sentiment_finbert) - min(weighted_sentiment_finbert)),
    norm_yiyanghkust = (weighted_sentiment_yiyanghkust - min(weighted_sentiment_yiyanghkust)) /
      (max(weighted_sentiment_yiyanghkust) - min(weighted_sentiment_yiyanghkust)),
    mean_weighted_sentiment = (weighted_sentiment_finbert + weighted_sentiment_yiyanghkust) / 2,
    norm_mean_sentiment = (mean_weighted_sentiment - min(mean_weighted_sentiment)) /
      (max(mean_weighted_sentiment) - min(mean_weighted_sentiment))
  )

data$DATE <- as.Date(data$DATE)
icc$DATE <- as.Date(icc$DATE)

data <- left_join(data, icc, by = "DATE")

for (i in 2:6) {
  new_colname <- paste0(names(data)[2], "_", i)
  new_colname2 <- paste0(names(data)[3], "_", i)
  new_colname6 <- paste0(names(data)[6], "_", i)
  new_colname7 <- paste0(names(data)[7], "_", i)
  new_colname8 <- paste0(names(data)[9], "_", i)
  data[[new_colname]] <- zoo::rollmean(data$weighted_sentiment_finbert, k = i, fill = NA, align = "right")
  data[[new_colname2]] <- zoo::rollmean(data$weighted_sentiment_yiyanghkust, k = i, fill = NA, align = "right")
  data[[new_colname6]] <- zoo::rollmean(data$norm_finbert, k = i, fill = NA, align = "right")
  data[[new_colname7]] <- zoo::rollmean(data$norm_yiyanghkust, k = i, fill = NA, align = "right")
  data[[new_colname8]] <- zoo::rollmean(data$norm_mean_sentiment, k = i, fill = NA, align = "right")
}

data2 <- data %>%
  # filter(DATE < as.Date('2025-01-01') & DATE >= as.Date('2020-01-01')) %>% 
  mutate(covid = ifelse(DATE < as.Date('2021-07-01') & DATE > as.Date('2020-03-01'), 1, 0)) %>% 
  na.omit() 

data2 <- data2 %>% 
  mutate(
    icc = as.numeric(sub(",", ".", icc, fixed = TRUE))
  )

plot(data2$icc, type = 'l')
plot(data2$norm_mean_sentiment_4, type = 'l')

data2$icc_diff <- c(NA, diff(data2$icc))

saveRDS(data, 'C:/Users/gusta/Documents/GitHub/PhDEcon107/data/data_new.rds')
saveRDS(data2, 'C:/Users/gusta/Documents/GitHub/PhDEcon107/data/data2_new.rds')

rm(list = ls())
