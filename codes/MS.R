# Packages
library(tidyverse)
library(lubridate)
library(corrplot)
library(vars)
library(MSwM)
library(zoo)

setwd("~/Documents/GitHub/PhDEcon107/codes/")

icc <- read.csv2("~/Documents/GitHub/PhDEcon107/data/icc.csv")
icc <- icc %>%
  mutate(DATE = floor_date(as.Date(DATE), unit = "month"))

sentiment_scored_aggregated <- read.csv("~/Documents/GitHub/PhDEcon107/data/sentiment_scored_aggregated.csv")

sentiment_scored_aggregated$DATE <- as.Date(sentiment_scored_aggregated$DATE)

# wrangling
data <- sentiment_scored_aggregated %>%
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
  filter(DATE < as.Date('2025-01-01') & DATE >= as.Date('2020-01-01')) %>% 
  mutate(covid = ifelse(DATE < as.Date('2021-07-01') & DATE > as.Date('2020-03-01'), 1, 0)) %>% 
  na.omit() 

plot(data2$ICC, type = 'l')
plot(data2$norm_mean_sentiment_4, type = 'l')

data2$icc_diff <- c(NA, diff(data2$ICC))

lm_model <- lm(ICC ~ norm_finbert_5, data = data2, na.action = na.exclude)
lm_model <- lm(ICC ~ norm_mean_sentiment_5, data = data2, na.action = na.exclude)
lm_model <- lm(ICC ~ norm_yiyanghkust_5, data = data2, na.action = na.exclude)
# summary(lm_model)

ms_model <- msmFit(lm_model,
                   k = 2,
                   p = 0,
                   sw = c(TRUE, TRUE, TRUE),
                   control = list(parallel = FALSE, trace = TRUE))

summary(ms_model)
# plot(resid(ms_model@model), type='l')

plotProb(ms_model, which = 2)
