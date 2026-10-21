# rm(list = ls())
library(urca)
library(patchwork)
library(cowplot)

# data <- readRDS("~/Documents/GitHub/PhDEcon107/data/data2.rds")
data <- readRDS("C:/Users/gusta/Documents/GitHub/PhDEcon107/data/sent_all_new_final.rds")


# figure 2 ====
df <- data %>% filter(DATE >= as.Date("2010-01-01"))   # paper sample 2010–2025

# method -> base column name (k = 1 has no suffix; k = 2..6 use _k)
stems <- c("FinBERT-Prosus" = "weighted_sentiment_finbert",
           "FinBERT-Tone"   = "weighted_sentiment_yiyanghkust",
           "LM"             = "weighted_lm_sentiment")

reps <- c(1, 6)   # k = 1 (raw) vs k = 6 (max smoothing)

long <- map_dfr(names(stems), function(m) {
  map_dfr(reps, function(k) {
    col <- if (k == 1) stems[[m]] else paste0(stems[[m]], "_", k)
    tibble(DATE = df$DATE, method = m, k = k, value = df[[col]])
  })
}) %>%
  group_by(method, k) %>%
  mutate(z = as.numeric(scale(value))) %>%
  ungroup() %>%
  mutate(k = factor(k, levels = reps,
                    labels = c("k = 1 (raw)", "k = 6 (smoothed)")))

base_plot <- ggplot(long, aes(DATE, z, colour = method,
                              linetype = k, linewidth = k, alpha = k,
                              group = interaction(method, k))) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
  geom_line(lineend = "round") +
  scale_colour_manual(values = c("FinBERT-Prosus" = "#1b9e77",
                                 "FinBERT-Tone"   = "#d95f02",
                                 "LM"             = "#7570b3")) +
  scale_linetype_manual(values  = c("k = 1 (raw)" = "dashed", "k = 6 (smoothed)" = "solid")) +
  scale_linewidth_manual(values = c("k = 1 (raw)" = 0.35,     "k = 6 (smoothed)" = 1.1)) +
  scale_alpha_manual(values     = c("k = 1 (raw)" = 0.55,     "k = 6 (smoothed)" = 1)) +
  labs(x = "Year", y = "Standardized sentiment (z-score)",
       colour = "Method", linetype = "Window", linewidth = "Window", alpha = "Window") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor  = element_blank(),
        legend.background = element_blank(),
        legend.key        = element_blank(),
        axis.title.x = element_text(size = 15, colour = "black"),
        axis.title.y = element_text(size = 15, colour = "black"),
        axis.text.x  = element_text(size = 15, colour = "black"),
        axis.text.y  = element_text(size = 15, colour = "black"),
        legend.title      = element_text(size = 15),
        legend.text       = element_text(size = 15))

# painel sem legendas
main <- base_plot + theme(legend.position = "none")

# extrair cada legenda separadamente
# Method: forca linhas solidas e opacas nas chaves, so a cor varia
leg_method <- get_legend(
  base_plot +
    guides(linetype = "none", linewidth = "none", alpha = "none",
           colour = guide_legend(override.aes =
                                   list(linewidth = 2, alpha = 1, linetype = "solid"))) +
    theme(legend.position = "right")
)

# Window: linetype + linewidth + alpha compartilham o titulo "Window",
# entao o ggplot as funde numa unica legenda (tracejada fina vs solida grossa)
leg_window <- get_legend(base_plot + guides(colour = "none") +
                           theme(legend.position = "right"))

# compor: Method em cima à esquerda, Window em cima à direita
final <- ggdraw(main) +
  draw_grob(leg_method, x = 0.10, y = 0.74, width = 0.2, height = 0.2) +
  draw_grob(leg_window, x = 0.74, y = 0.74, width = 0.2, height = 0.2)
final





# reshape to long: one row per DATE × method × k
long <- map_dfr(names(stems), function(m) {
  map_dfr(reps, function(k) {
    col <- if (k == 1) stems[[m]] else paste0(stems[[m]], "_", k)
    tibble(DATE = df$DATE, method = m, k = k, value = df[[col]])
  })
}) %>%
  group_by(method, k) %>%
  mutate(z = as.numeric(scale(value))) %>%      # z-score PER method × k
  ungroup() %>%
  mutate(k = factor(k, levels = reps,
                    labels = c("k = 1 (raw)", "k = 6 (smoothed)")))

ggplot(long, aes(DATE, z, colour = method,
                 linewidth = k, alpha = k,
                 group = interaction(method, k))) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
  geom_line() +
  scale_colour_manual(values = c("FinBERT-Prosus" = "#EE6A50",
                                 "FinBERT-Tone"   = "#8EE5EE",
                                 "LM"             = "#EEC591")) +
  scale_linewidth_manual(values = c("k = 1 (raw)" = 0.3, "k = 6 (smoothed)" = 1.1)) +
  scale_alpha_manual(values     = c("k = 1 (raw)" = 0.35, "k = 6 (smoothed)" = 1)) +
  labs(x = NULL, y = "Standardized sentiment (z-score)",
       colour = "Method", linewidth = "Window", alpha = "Window") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())


#


reds <- c("#FFCCCC", "#FF6666", "#FF0000", "#CC0000", "#990000", "#660000")


# FinBERT ====
p_f <- data %>% 
  filter(DATE >= as.Date('2010-01-01')) %>% 
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
  filter(DATE >= as.Date('2010-01-01')) %>% 
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
  geom_line(aes(y = ICC), size = .8, colour = '#EE6A50') +
  labs(y = 'Consumer Confidence Indicator', x = "Year") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 15, colour = "black", angle = 90),
    axis.text.x  = element_text(size = 15, colour = "black"),
    axis.text.y  = element_text(size = 15, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 15, colour = "black")
  )


p_f + p_y 



# correlations ====

cor(data$weighted_sentiment_finbert, data$ICC)
cor(data$weighted_sentiment_finbert_2, data$ICC)
cor(data$weighted_sentiment_finbert_3, data$ICC)
cor(data$weighted_sentiment_finbert_4, data$ICC)
cor(data$weighted_sentiment_finbert_5, data$ICC)
cor(data$weighted_sentiment_finbert_6, data$ICC)

cor(data$weighted_sentiment_yiyanghkust  , data$ICC)
cor(data$weighted_sentiment_yiyanghkust_2, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_3, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_4, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_5, data$ICC)
cor(data$weighted_sentiment_yiyanghkust_6, data$ICC)

cor(data$weighted_lm_sentiment  , data$ICC)
cor(data$weighted_lm_sentiment_2, data$ICC)
cor(data$weighted_lm_sentiment_3, data$ICC)
cor(data$weighted_lm_sentiment_4, data$ICC)
cor(data$weighted_lm_sentiment_5, data$ICC)
cor(data$weighted_lm_sentiment_6, data$ICC)

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


plotProb(models_list[["weighted_sentiment_finbert_lag_3_std"]], which = 2)
