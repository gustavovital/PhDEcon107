# rm(list = ls())
library(urca)
library(patchwork)

data <- urcadata <- readRDS("~/Documents/GitHub/PhDEcon107/data/data2.rds")

reds <- c("#FFCCCC", "#FF6666", "#FF0000", "#CC0000", "#990000", "#660000")


# FinBERT ====
p_f <- data %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = weighted_sentiment_finbert,   colour = "FinBERT (k = 1)"), alpha = 0.55, size = 1.2) +
  geom_line(aes(y = weighted_sentiment_finbert_2, colour = "FinBERT (k = 2)"), alpha = 0.55, size = 1.2) +
  geom_line(aes(y = weighted_sentiment_finbert_3, colour = "FinBERT (k = 3)"), alpha = 0.55, size = 1.2) +
  geom_line(aes(y = weighted_sentiment_finbert_4, colour = "FinBERT (k = 4)"), alpha = 0.55, size = 1.2) +
  geom_line(aes(y = weighted_sentiment_finbert_5, colour = "FinBERT (k = 5)"), alpha = 0.55, size = 1.2) +
  geom_line(aes(y = weighted_sentiment_finbert_6, colour = "FinBERT (k = 6)"), alpha = 0.55, size = 1.2) +
  scale_colour_manual(values = reds, name = NULL) +          # no legend title
  theme_minimal() +
  theme(
    legend.position = "bottom",                              # legend below plot
    axis.title.x  = element_blank(),                         # remove x-axis title
    axis.title.y  = element_blank()                          # remove y-axis title
  ) 
  

# yiyanghkust ====
blues <- c("#CCE5FF", "#99CCFF", "#66B2FF", "#3399FF", "#007FFF", "#0059B3")

p_y <- data %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = weighted_sentiment_yiyanghkust,   colour = "Yiyanghkust (k = 1)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_sentiment_yiyanghkust_2, colour = "Yiyanghkust (k = 2)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_sentiment_yiyanghkust_3, colour = "Yiyanghkust (k = 3)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_sentiment_yiyanghkust_4, colour = "Yiyanghkust (k = 4)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_sentiment_yiyanghkust_5, colour = "Yiyanghkust (k = 5)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_sentiment_yiyanghkust_6, colour = "Yiyanghkust (k = 6)"), alpha = 0.55, size =1.2) +
  scale_colour_manual(values = blues, name = NULL) +          # no legend title
  theme_minimal() +
  theme(
    legend.position = "bottom",                              # legend below plot
    axis.title.x  = element_blank(),                         # remove x-axis title
    axis.title.y  = element_blank()                          # remove y-axis title
  )

p_icc <- data %>% 
  ggplot(aes(x = DATE)) + 
  geom_line(aes(y = ICC), size = 1.2, colour = 'gray60') +
  theme_minimal() +
  theme(
    legend.position = "bottom",                              # legend below plot
    axis.title.x  = element_blank(),                         # remove x-axis title
    axis.title.y  = element_blank()                          # remove y-axis title
  )


p_f + p_y 



# correlations ====
cor(data$weighted_sentiment_finbert, data$ICC)
cor(data$weighted_sentiment_finbert_2, data$ICC)
cor(data$weighted_sentiment_finbert_3, data$ICC)
cor(data$weighted_sentiment_finbert_4, data$ICC)
cor(data$weighted_sentiment_finbert_5, data$ICC)
cor(data$weighted_sentiment_finbert_6, data$ICC)

cor(data$weighted_sentiment_yiyanghkust, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_2, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_3, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_4, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_5, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_6, data$ICC)

cor(data$weighted_sentiment_yiyanghkust, data$weighted_sentiment_finbert)
cor(data$weighted_sentiment_yiyanghkust_2, data$weighted_sentiment_finbert_2)
cor(data$weighted_sentiment_yiyanghkust_3, data$weighted_sentiment_finbert_3)
cor(data$weighted_sentiment_yiyanghkust_4, data$weighted_sentiment_finbert_4)
cor(data$weighted_sentiment_yiyanghkust_5, data$weighted_sentiment_finbert_5)
cor(data$weighted_sentiment_yiyanghkust_6, data$weighted_sentiment_finbert_6)

plotProb(models_list[["norm_finbert_lag_4"]], which = 1)
plotProb(models_list[["norm_finbert_lag_4"]], which = 2)

plotProb(models_list[["norm_yiyanghkust_lag_3"]], which = 1)
plotProb(models_list[["norm_yiyanghkust_lag_3"]], which = 2)


