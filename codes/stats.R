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

data %>% 
  filter(DATE >= as.Date('2010-01-01')) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = weighted_lm_sentiment,   colour = "Yiyanghkust (k = 1)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_lm_sentiment_2, colour = "Yiyanghkust (k = 2)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_lm_sentiment_3, colour = "Yiyanghkust (k = 3)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_lm_sentiment_4, colour = "Yiyanghkust (k = 4)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_lm_sentiment_5, colour = "Yiyanghkust (k = 5)"), alpha = 0.55, size =1.2) +
  geom_line(aes(y = weighted_lm_sentiment_6, colour = "Yiyanghkust (k = 6)"), alpha = 0.55, size =1.2) +
  scale_colour_manual(values = blues, name = NULL) +          # no legend title
  theme_minimal() +
  theme(
    legend.position = "bottom",                              # legend below plot
    axis.title.x  = element_blank(),                         # remove x-axis title
    axis.title.y  = element_blank()                          # remove y-axis title
  )


## teste 
# y_min <- min(data %>% 
#                dplyr::select(starts_with("weighted_lm_sentiment")) %>% 
#                unlist(), na.rm = TRUE)
# 
# y_max <- max(data %>% 
#                dplyr::select(starts_with("weighted_lm_sentiment")) %>% 
#                unlist(), na.rm = TRUE)

## ENVELOP DATA ====
data_envelope_lm <- data %>%
  mutate(
    ymin = pmin(weighted_lm_sentiment,
                weighted_lm_sentiment_2,
                weighted_lm_sentiment_3,
                weighted_lm_sentiment_4,
                weighted_lm_sentiment_5,
                weighted_lm_sentiment_6,
                na.rm = TRUE),
    ymax = pmax(weighted_lm_sentiment,
                weighted_lm_sentiment_2,
                weighted_lm_sentiment_3,
                weighted_lm_sentiment_4,
                weighted_lm_sentiment_5,
                weighted_lm_sentiment_6,
                na.rm = TRUE)
  )


g_lm <- ggplot(data_envelope_lm, aes(x = DATE)) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax, fill = "LM (k = 1–6)"),
    alpha = .9
  ) +
  scale_fill_manual(
    values = c("LM (k = 1–6)" = "#76EEC6"),
    name = NULL
  ) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 13, colour = "black")
  )

data_envelope_finbert <- data %>%
  mutate(
    ymin = pmin(weighted_sentiment_finbert,
                weighted_sentiment_finbert_2,
                weighted_sentiment_finbert_3,
                weighted_sentiment_finbert_4,
                weighted_sentiment_finbert_5,
                weighted_sentiment_finbert_6,
                na.rm = TRUE),
    ymax = pmax(weighted_sentiment_finbert,
                weighted_sentiment_finbert_2,
                weighted_sentiment_finbert_3,
                weighted_sentiment_finbert_4,
                weighted_sentiment_finbert_5,
                weighted_sentiment_finbert_6,
                na.rm = TRUE)
  )

g_fin <- ggplot(data_envelope_finbert, aes(x = DATE)) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax, fill = "FinBERT (k = 1–6)"),
    alpha = .9
  ) +
  scale_fill_manual(
    values = c("FinBERT (k = 1–6)" = "#EEC591"),
    name = NULL
  ) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 13, colour = "black")
  )

data_envelope_yiyang <- data %>%
  mutate(
    ymin = pmin(weighted_sentiment_yiyanghkust,
                weighted_sentiment_yiyanghkust_2,
                weighted_sentiment_yiyanghkust_3,
                weighted_sentiment_yiyanghkust_4,
                weighted_sentiment_yiyanghkust_5,
                weighted_sentiment_yiyanghkust_6,
                na.rm = TRUE),
    ymax = pmax(weighted_sentiment_yiyanghkust,
                weighted_sentiment_yiyanghkust_2,
                weighted_sentiment_yiyanghkust_3,
                weighted_sentiment_yiyanghkust_4,
                weighted_sentiment_yiyanghkust_5,
                weighted_sentiment_yiyanghkust_6,
                na.rm = TRUE)
  )

g_yi <- ggplot(data_envelope_yiyang, aes(x = DATE)) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax, fill = "Yiyanghkust (k = 1–6)"),
    alpha = .9
  ) +
  scale_fill_manual(
    values = c("Yiyanghkust (k = 1–6)" = "#8EE5EE"),
    name = NULL
  ) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 13, colour = "black")
  )

g_lm / g_fin / g_yi

####

data %>% 
  ggplot(aes(x = DATE)) + 
  geom_line(aes(y = icc), size = .8, colour = '#EE6A50') +
  theme_minimal() +
  theme(
    legend.position = "bottom",                              # legend below plot
    axis.title.x  = element_blank(),                         # remove x-axis title
    axis.title.y  = element_blank()                          # remove y-axis title
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 13, colour = "black")
  )


p_f + p_y 



# correlations ====

cor(data$weighted_sentiment_finbert, data$icc)
cor(data$weighted_sentiment_finbert_2, data$icc)
cor(data$weighted_sentiment_finbert_3, data$icc)
cor(data$weighted_sentiment_finbert_4, data$icc)
cor(data$weighted_sentiment_finbert_5, data$icc)
cor(data$weighted_sentiment_finbert_6, data$icc)

cor(data$weighted_sentiment_yiyanghkust  , data$icc)
cor(data$weighted_sentiment_yiyanghkust_2, data$icc)
cor(data$weighted_sentiment_yiyanghkust_3, data$icc)
cor(data$weighted_sentiment_yiyanghkust_4, data$icc)
cor(data$weighted_sentiment_yiyanghkust_5, data$icc)
cor(data$weighted_sentiment_yiyanghkust_6, data$icc)

cor(data$weighted_lm_sentiment  , data$icc)
cor(data$weighted_lm_sentiment_2, data$icc)
cor(data$weighted_lm_sentiment_3, data$icc)
cor(data$weighted_lm_sentiment_4, data$icc)
cor(data$weighted_lm_sentiment_5, data$icc)
cor(data$weighted_lm_sentiment_6, data$icc)

cor(data$weighted_sentiment_finbert,   data$weighted_sentiment_yiyanghkust  )
cor(data$weighted_sentiment_finbert_2, data$weighted_sentiment_yiyanghkust_2)
cor(data$weighted_sentiment_finbert_3, data$weighted_sentiment_yiyanghkust_3)
cor(data$weighted_sentiment_finbert_4, data$weighted_sentiment_yiyanghkust_4)
cor(data$weighted_sentiment_finbert_5, data$weighted_sentiment_yiyanghkust_5)
cor(data$weighted_sentiment_finbert_6, data$weighted_sentiment_yiyanghkust_6)

cor(data$weighted_sentiment_finbert  ,   data$weighted_lm_sentiment)
cor(data$weighted_sentiment_finbert_2, data$weighted_lm_sentiment_2)
cor(data$weighted_sentiment_finbert_3, data$weighted_lm_sentiment_3)
cor(data$weighted_sentiment_finbert_4, data$weighted_lm_sentiment_4)
cor(data$weighted_sentiment_finbert_5, data$weighted_lm_sentiment_5)
cor(data$weighted_sentiment_finbert_6, data$weighted_lm_sentiment_6) 

cor(data$weighted_sentiment_yiyanghkust  ,   data$weighted_lm_sentiment)
cor(data$weighted_sentiment_yiyanghkust_2, data$weighted_lm_sentiment_2)
cor(data$weighted_sentiment_yiyanghkust_3, data$weighted_lm_sentiment_3)
cor(data$weighted_sentiment_yiyanghkust_4, data$weighted_lm_sentiment_4)
cor(data$weighted_sentiment_yiyanghkust_5, data$weighted_lm_sentiment_5)
cor(data$weighted_sentiment_yiyanghkust_6, data$weighted_lm_sentiment_6)



  # cor(data$weighted_sentiment_finbert, data$ICC)
  # cor(data$weighted_sentiment_finbert_2, data$ICC)
  # cor(data$weighted_sentiment_finbert_3, data$ICC)
  # cor(data$weighted_sentiment_finbert_4, data$ICC)
  # cor(data$weighted_sentiment_finbert_5, data$ICC)
  # cor(data$weighted_sentiment_finbert_6, data$ICC)
  # 
  # cor(data$weighted_sentiment_yiyanghkust, data$ICC)
  # cor(data$weighted_sentiment_yiyanghkust_2, data$ICC)
  # cor(data$weighted_sentiment_yiyanghkust_3, data$ICC)
  # cor(data$weighted_sentiment_yiyanghkust_4, data$ICC)
  # cor(data$weighted_sentiment_yiyanghkust_5, data$ICC)
  # cor(data$weighted_sentiment_yiyanghkust_6, data$ICC)
  # 
  # cor(data$weighted_sentiment_yiyanghkust, data$weighted_sentiment_finbert)
  # cor(data$weighted_sentiment_yiyanghkust_2, data$weighted_sentiment_finbert_2)
  # cor(data$weighted_sentiment_yiyanghkust_3, data$weighted_sentiment_finbert_3)
  # cor(data$weighted_sentiment_yiyanghkust_4, data$weighted_sentiment_finbert_4)
  # cor(data$weighted_sentiment_yiyanghkust_5, data$weighted_sentiment_finbert_5)
  # cor(data$weighted_sentiment_yiyanghkust_6, data$weighted_sentiment_finbert_6)

plotProb(models_list[["norm_finbert_lag_4_std"]], which = 1)
plotProb(models_list[["norm_finbert_lag_4"]], which = 2)

plotProb(models_list[["norm_yiyanghkust_lag_3"]], which = 1)
plotProb(models_list[["norm_yiyanghkust_lag_3"]], which = 2)


