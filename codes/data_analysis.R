# Packages
library(tidyverse)
library(lubridate)
library(corrplot)
library(vars)

setwd("~/Documents/GitHub/PhDEcon107/codes/")

icc <- read.csv2("~/Documents/GitHub/PhDEcon107/codes/icc.csv")
icc <- icc %>%
  mutate(DATE = floor_date(as.Date(DATE), unit = "month"))

sentiment_scored_aggregated <- read.csv("~/Documents/GitHub/PhDEcon107/codes/sentiment_scored_aggregated.csv")

# A bit of wrangling
sentiment_scored_aggregated[, -ncol(sentiment_scored_aggregated)] -> sentiment_scored_aggregated
sentiment_scored_aggregated$DATE <- as.Date(sentiment_scored_aggregated$DATE)


data <- sentiment_scored_aggregated %>%
  mutate(DATE = floor_date(DATE, unit = "month")) %>%
  group_by(DATE) %>%
  summarise(
    sentiment_finbert = mean(sentiment_finbert, na.rm = TRUE),
    sentiment_yiyanghkust = mean(sentiment_yiyanghkust, na.rm = TRUE)
  ) %>%
  mutate(
    sentiment_f_normalised = (sentiment_finbert - min(sentiment_finbert, na.rm = TRUE)) /
      (max(sentiment_finbert, na.rm = TRUE) - min(sentiment_finbert, na.rm = TRUE)),
    sentiment_y_normalised = (sentiment_yiyanghkust - min(sentiment_yiyanghkust, na.rm = TRUE)) /
      (max(sentiment_yiyanghkust, na.rm = TRUE) - min(sentiment_yiyanghkust, na.rm = TRUE))
  ) %>% 
  mutate(avg_norm = (sentiment_f_normalised + sentiment_y_normalised)/2)

data$DATE <- as.Date(data$DATE)
icc$DATE <- as.Date(icc$DATE)

data <- left_join(data, icc, by = "DATE")

data$sentiment_f_normalised2 <- zoo::rollmean(data$sentiment_f_normalised, k = 2, fill = NA, align = "right")
data$sentiment_f_normalised3<- zoo::rollmean(data$sentiment_f_normalised, k = 3, fill = NA, align = "right")
data$sentiment_f_normalised4 <- zoo::rollmean(data$sentiment_f_normalised, k = 4, fill = NA, align = "right")
data$sentiment_f_normalised5 <- zoo::rollmean(data$sentiment_f_normalised, k = 5, fill = NA, align = "right")
data$sentiment_f_normalised6 <- zoo::rollmean(data$sentiment_f_normalised, k = 6, fill = NA, align = "right")

data$sentiment_y_normalised2 <- zoo::rollmean(data$sentiment_y_normalised, k = 2, fill = NA, align = "right")
data$sentiment_y_normalised3 <- zoo::rollmean(data$sentiment_y_normalised, k = 3, fill = NA, align = "right")
data$sentiment_y_normalised4 <- zoo::rollmean(data$sentiment_y_normalised, k = 4, fill = NA, align = "right")
data$sentiment_y_normalised5 <- zoo::rollmean(data$sentiment_y_normalised, k = 5, fill = NA, align = "right")
data$sentiment_y_normalised6 <- zoo::rollmean(data$sentiment_y_normalised, k = 6, fill = NA, align = "right")

data$avg_norm2 <- zoo::rollmean(data$avg_norm, k = 2, fill = NA, align = "right")
data$avg_norm3 <- zoo::rollmean(data$avg_norm, k = 3, fill = NA, align = "right")
data$avg_norm4 <- zoo::rollmean(data$avg_norm, k = 4, fill = NA, align = "right")
data$avg_norm5 <- zoo::rollmean(data$avg_norm, k = 5, fill = NA, align = "right")
data$avg_norm6 <- zoo::rollmean(data$avg_norm, k = 6, fill = NA, align = "right")

# data analysis
summary(data)

# var
install.packages("MSwM")
library(MSwM)

# Modelo base
data2 <- data %>%
  filter(DATE < as.Date('2025-01-01') & DATE >= as.Date('2018-01-01')) %>% 
  mutate(covid = ifelse(DATE < as.Date('2021-07-01') & DATE > as.Date('2020-03-01'), 1, 0)) %>% 
  na.omit() 
# data2 <- data %>%
#   filter(DATE < as.Date('2025-01-01') & DATE >= as.Date('2018-01-01')) %>% 
#   mutate(covid = ifelse(DATE < as.Date('2021-12-01') & DATE > as.Date('2020-03-01'), 1, 0)) %>% 
#   na.omit() 

data2$icc_diff <- c(NA, diff(data2$ICC))

lm_model <- lm(icc_diff ~ sentiment_y_normalised + covid, data = data2, na.action = na.exclude)
# lm_model <- lm(ICC ~ sentiment_y_normalised4 + covid, data = data2, na.action = na.exclude) # ALL GOOD TOO
# lm_model <- lm(ICC ~ sentiment_y_normalised5 + covid, data = data2, na.action = na.exclude) # intercepto nao sig, msm quando covid dummy muda
# lm_model <- lm(ICC ~ sentiment_y_normalised4 + covid, data = data2, na.action = na.exclude) # intercepto nap sig
# lm_model <- lm(ICC ~ sentiment_y_normalised3, data = data2, na.action = na.exclude) # significante em tudo

# Modelo com Markov Switching: 2 regimes
ms_model <- msmFit(lm_model,
                   k = 2,
                   p = 0,
                   sw = c( TRUE, TRUE, TRUE, TRUE),
                   control = list(parallel = FALSE, trace = TRUE))

summary(ms_model)
plotProb(ms_model, which = 1)  # Probabilidade de estar no regime 1


## VAR ====
var_data <- na.omit(data2[, c("ICC", "sentiment_f_normalised5")])
exog_data <- na.omit(data2[, "covid", drop = FALSE])  # precisa ser data.frame

# Estime o número de defasagens ideais
VARselect(var_data, lag.max = 6, type = "const")

# Estime o VAR com exógeno
var_model <- VAR(y = var_data, p = 1, type = "const")
summary(var_model)

irf_model <- irf(var_model, impulse = "sentiment_f_normalised5", response = "ICC", n.ahead = 12, boot = TRUE)
plot(irf_model)

stab_check <- stability(var_model, type = "OLS-CUSUM")
plot(stab_check)
