# Packages
library(tidyverse)
library(lubridate)
library(corrplot)
library(vars)
library(MSwM)

setwd("~/Documents/GitHub/PhDEcon107/codes/")

icc <- read.csv2("~/Documents/GitHub/PhDEcon107/data/icc.csv")
icc <- icc %>%
  mutate(DATE = floor_date(as.Date(DATE), unit = "month"))

sentiment_scored_aggregated <- read.csv("~/Documents/GitHub/PhDEcon107/data/sentiment_scored_aggregated.csv")

# A bit of wrangling
# sentiment_scored_aggregated[, -ncol(sentiment_scored_aggregated)] -> sentiment_scored_aggregated
sentiment_scored_aggregated$DATE <- as.Date(sentiment_scored_aggregated$DATE)

library(dplyr)
library(lubridate)

# Agrupar e calcular sentimentos ponderados
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
    
  )


# data <- sentiment_scored_aggregated %>%
#   mutate(DATE = floor_date(DATE, unit = "month")) %>%
#   group_by(DATE) %>%
#   summarise(
#     sentiment_finbert = mean(sentiment_finbert, na.rm = TRUE),
#     sentiment_yiyanghkust = mean(sentiment_yiyanghkust, na.rm = TRUE)
#   ) %>%
#   mutate(
#     sentiment_f_normalised = (sentiment_finbert - min(sentiment_finbert, na.rm = TRUE)) /
#       (max(sentiment_finbert, na.rm = TRUE) - min(sentiment_finbert, na.rm = TRUE)),
#     sentiment_y_normalised = (sentiment_yiyanghkust - min(sentiment_yiyanghkust, na.rm = TRUE)) /
#       (max(sentiment_yiyanghkust, na.rm = TRUE) - min(sentiment_yiyanghkust, na.rm = TRUE))
#   ) %>% 
#   mutate(avg_norm = (sentiment_f_normalised + sentiment_y_normalised)/2)

data$DATE <- as.Date(data$DATE)
icc$DATE <- as.Date(icc$DATE)

data <- left_join(data, icc, by = "DATE")

library(zoo)

for (i in 2:6) {
  new_colname <- paste0(names(data)[2], "_", i)
  new_colname2 <- paste0(names(data)[3], "_", i)
  new_colname6 <- paste0(names(data)[6], "_", i)
  new_colname7 <- paste0(names(data)[7], "_", i)
  data[[new_colname]] <- zoo::rollmean(data$weighted_sentiment_finbert, k = i, fill = NA, align = "right")
  data[[new_colname2]] <- zoo::rollmean(data$weighted_sentiment_yiyanghkust, k = i, fill = NA, align = "right")
  data[[new_colname6]] <- zoo::rollmean(data$norm_finbert, k = i, fill = NA, align = "right")
  data[[new_colname7]] <- zoo::rollmean(data$norm_yiyanghkust, k = i, fill = NA, align = "right")
}

# 
# data$sentiment_f_normalised2 <- zoo::rollmean(data$sentiment_f_normalised, k = 2, fill = NA, align = "right")
# data$sentiment_f_normalised3<- zoo::rollmean(data$sentiment_f_normalised, k = 3, fill = NA, align = "right")
# data$sentiment_f_normalised4 <- zoo::rollmean(data$sentiment_f_normalised, k = 4, fill = NA, align = "right")
# data$sentiment_f_normalised5 <- zoo::rollmean(data$sentiment_f_normalised, k = 5, fill = NA, align = "right")
# data$sentiment_f_normalised6 <- zoo::rollmean(data$sentiment_f_normalised, k = 6, fill = NA, align = "right")
# 
# data$sentiment_y_normalised2 <- zoo::rollmean(data$sentiment_y_normalised, k = 2, fill = NA, align = "right")
# data$sentiment_y_normalised3 <- zoo::rollmean(data$sentiment_y_normalised, k = 3, fill = NA, align = "right")
# data$sentiment_y_normalised4 <- zoo::rollmean(data$sentiment_y_normalised, k = 4, fill = NA, align = "right")
# data$sentiment_y_normalised5 <- zoo::rollmean(data$sentiment_y_normalised, k = 5, fill = NA, align = "right")
# data$sentiment_y_normalised6 <- zoo::rollmean(data$sentiment_y_normalised, k = 6, fill = NA, align = "right")
# 
# data$avg_norm2 <- zoo::rollmean(data$avg_norm, k = 2, fill = NA, align = "right")
# data$avg_norm3 <- zoo::rollmean(data$avg_norm, k = 3, fill = NA, align = "right")
# data$avg_norm4 <- zoo::rollmean(data$avg_norm, k = 4, fill = NA, align = "right")
# data$avg_norm5 <- zoo::rollmean(data$avg_norm, k = 5, fill = NA, align = "right")
# data$avg_norm6 <- zoo::rollmean(data$avg_norm, k = 6, fill = NA, align = "right")

# data analysis
# summary(data)

# var

# Modelo base
data2 <- data %>%
  filter(DATE < as.Date('2025-01-01') & DATE >= as.Date('2020-04-01')) %>% 
  mutate(covid = ifelse(DATE < as.Date('2021-07-01') & DATE > as.Date('2020-03-01'), 1, 0)) %>% 
  na.omit() 
# data2 <- data %>%
#   filter(DATE < as.Date('2025-01-01') & DATE >= as.Date('2018-01-01')) %>% 
#   mutate(covid = ifelse(DATE < as.Date('2021-12-01') & DATE > as.Date('2020-03-01'), 1, 0)) %>% 
#   na.omit() 

plot(data2$ICC, type = 'l')

data2$icc_diff <- c(NA, diff(data2$ICC))


lm_model <- lm(icc_diff ~ weighted_sentiment_finbert_5 + covid, data = data2, na.action = na.exclude)
lm_model <- lm(icc_diff ~ norm_finbert_5 + covid, data = data2, na.action = na.exclude)
lm_model <- lm(ICC ~ weighted_sentiment_finbert_5 + weighted_sentiment_yiyanghkust_3 + covid, data = data2, na.action = na.exclude)
lm_model <- lm(ICC ~ weighted_sentiment_finbert_2, data = data2, na.action = na.exclude)

# lm_model <- lm(icc_diff ~ weighted_sentiment_yiyanghkust_6 + covid, data = data2, na.action = na.exclude)

ms_model <- msmFit(lm_model,
                   k = 2,
                   p = 0,
                   sw = c( TRUE, TRUE, TRUE),
                   control = list(parallel = FALSE, trace = TRUE))

summary(ms_model)
plotProb(ms_model, which = 2)  # Probabilidade de estar no regime 1


plot(data$weighted_sentiment_finbert_4, type = 'l')






# lm_model <- lm(icc_diff ~ sentiment_y_normalised + covid, data = data2, na.action = na.exclude)
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
var_data <- na.omit(data2[, c("ICC", "sentiment_f_normalised4")])
exog_data <- na.omit(data2[, "covid", drop = FALSE])  # precisa ser data.frame

# Estime o número de defasagens ideais
VARselect(var_data, lag.max = 6, type = "const", exogen = exog_data)

# VAR com exógeno
var_model <- VAR(y = var_data, p = 5, type = "const", exogen = exog_data)
summary(var_model)

irf_model <- irf(var_model, impulse = "sentiment_f_normalised4", response = "ICC", n.ahead = 72, boot = TRUE)
plot(irf_model)

stab_check <- stability(var_model, type = "OLS-CUSUM")
plot(stab_check)

####################################
# ===== Previsão com modelo MS (1 passo à frente) =====

# Última observação disponível
last_obs <- tail(data2, 1)

# Pegando as probabilidades suavizadas do último tempo t
last_probs <- tail(ms_model@Fit@smoProb, 1)
P <- ms_model@transMat

# Probabilidades previstas para t+1
predicted_probs <- as.vector(last_probs %*% P)

# Coeficientes por regime (intercepto, sentiment, covid)
coefs <- ms_model@Coef

# Nova observação para previsão (t+1)
X_new <- c(1,  # intercept
           last_obs$sentiment_y_normalised4,
           last_obs$covid)  # ou colocar 0 se achar que covid acabou

# Previsão condicional em cada regime
regime_preds <- apply(coefs, 2, function(b) sum(b * X_new))

# Previsão final (ponderada pela probabilidade de cada regime em t+1)
forecast_ms <- sum(predicted_probs * regime_preds)

cat("Previsão (1 passo à frente) com modelo MS:", round(forecast_ms, 2), "\n")

#########
# Pegando as previsões condicionais por regime (n linhas x k regimes)
cond_preds <- ms_model@Fit@CondMean  # [n x regimes]
probs <- ms_model@Fit@smoProb[-1, ]  # Retira a primeira linha (NA) e alinha com cond_preds

# Previsão final ponderada pela probabilidade de cada regime
in_sample_preds <- rowSums(cond_preds * probs)

# Adiciona ao dataset
data2$ms_pred <- in_sample_preds

# Plot comparativo
library(ggplot2)
ggplot(data2, aes(x = DATE)) +
  geom_line(aes(y = ICC), color = "black", size = 1) +
  geom_line(aes(y = ms_pred), color = "blue", linetype = "dashed") +
  labs(title = "Previsão in-sample com modelo Markov Switching",
       y = "ICC",
       x = "Data") +
  theme_minimal()

# Erros
res_ms <- data2$ICC - data2$ms_pred
rmse <- sqrt(mean(res_ms^2, na.rm = TRUE))
mae <- mean(abs(res_ms), na.rm = TRUE)

cat("RMSE in-sample:", round(rmse, 3), "\n")
cat("MAE in-sample:", round(mae, 3), "\n")

##########
#######
reg1 <- lm(ICC ~ PIB + desemprego + IPCA, data = data2)
res_ICC <- resid(reg1)

reg2 <- lm(sentiment_y_normalised4 ~ PIB + desemprego + IPCA, data = data2)
res_sentiment <- resid(reg2)

reg3 <- lm(res_ICC ~ res_sentiment)
summary(reg3)