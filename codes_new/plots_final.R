library(dplyr)
library(tidyr)
library(ggplot2)

mod <- models_list_p1[["weighted_sentiment_finbert_lag_5_std"]]
# mod <- models_list[["weighted_sentiment_finbert_lag_6_std"]]
# mod <- models_list[["weighted_lm_sentiment_lag_6_std"]]

prob_data <- data.frame(
  t = 1:nrow(mod@Fit@filtProb),
  smoothed_1 = mod@Fit@smoProb[-1,1],
  smoothed_2 = mod@Fit@smoProb[-1,2],
  filtered_1 = mod@Fit@filtProb[,1],
  filtered_2 = mod@Fit@filtProb[,2]
)

prob_data$DATE <- data$DATE[1:nrow(prob_data)]


p_1 <- ggplot(prob_data, aes(x = DATE)) +
  geom_col(aes(y = filtered_1),
           fill = "#DEB887", width = 20) +
  geom_line(aes(y = smoothed_1),
            colour = "tomato2", size = .6) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Normal Regime",
       y = "Probabilities",
       x = NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 15, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black")
  )

p_2 <- 
  ggplot(prob_data, aes(x = DATE)) +
  geom_col(aes(y = filtered_2),
           fill = "#DEB887", width = 20, alpha =.8) +
  geom_line(aes(y = smoothed_2),
            colour = "tomato2", size = .6) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Uncertainty Regime",
       y = "Probabilities",
       x = "Year") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 15, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black")
  )

prob_data$regime1_dummy <- ifelse(prob_data$smoothed_1 > 0.5, 1, 0)

icc_plot_data <- tibble(
  DATE   = data$DATE[1:nrow(prob_data)],
  ICC    = data$ICC[1:nrow(prob_data)],
  regime = prob_data$regime1_dummy
) %>%
  arrange(DATE) %>%
  mutate(
    xmin = DATE,
    xmax = lead(DATE)
  )

p_icc <- ggplot(icc_plot_data, aes(x = DATE)) +
  geom_rect(
    data = filter(icc_plot_data, regime == 0, !is.na(xmax)),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "#EEC591",
    inherit.aes = FALSE,
    alpha = .5
  ) +
  labs(y = 'Consumer confidence (balance of responses)', x = "Year") + 
  geom_line(aes(y = ICC), colour = "black", linewidth=1) +
  #labs(title = "Regime 1", y = "ICC", x = NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black"),
    axis.text.x  = element_text(size = 15, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 13, colour = "black")
  )

(p_1 / p_2) 

p_icc

### LM ####

mod <- models_list_p1[["weighted_lm_sentiment_lag_5_std"]]

prob_data <- data.frame(
  t = 1:nrow(mod@Fit@filtProb),
  smoothed_1 = mod@Fit@smoProb[-1,1],
  smoothed_2 = mod@Fit@smoProb[-1,2],
  filtered_1 = mod@Fit@filtProb[,1],
  filtered_2 = mod@Fit@filtProb[,2]
)

prob_data$DATE <- data$DATE[1:nrow(prob_data)]


p_1 <- ggplot(prob_data, aes(x = DATE)) +
  geom_col(aes(y = filtered_1),
           fill = "#EE6A50", width = 20) +
  geom_line(aes(y = smoothed_1),
            colour = "tomato2", size = .6) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Normal Regime",
       y = "Probabilities",
       x = NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 15, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black")
  )

p_2 <- 
  ggplot(prob_data, aes(x = DATE)) +
  geom_col(aes(y = filtered_2),
           fill = "#EE6A50", width = 20, alpha =.8) +
  geom_line(aes(y = smoothed_2),
            colour = "tomato2", size = .6) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Uncertainty Regime",
       y = "Probabilities",
       x = "Year") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 15, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black")
  )

prob_data$regime1_dummy <- ifelse(prob_data$smoothed_1 > 0.5, 1, 0)

icc_plot_data <- tibble(
  DATE   = data$DATE[1:nrow(prob_data)],
  ICC    = data$ICC[1:nrow(prob_data)],
  regime = prob_data$regime1_dummy
) %>%
  arrange(DATE) %>%
  mutate(
    xmin = DATE,
    xmax = lead(DATE)
  )

p_icc <- ggplot(icc_plot_data, aes(x = DATE)) +
  geom_rect(
    data = filter(icc_plot_data, regime == 0, !is.na(xmax)),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "#EE6A50",
    inherit.aes = FALSE,
    alpha = .5
  ) +
  labs(y = 'Consumer confidence (balance of responses)', x = "Year") + 
  geom_line(aes(y = ICC), colour = "black", linewidth=1) +
  #labs(title = "Regime 1", y = "ICC", x = NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, colour = "black"),,
    axis.title.y = element_text(size = 16, colour = "black"),
    axis.text.x  = element_text(size = 15, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 13, colour = "black")
  )
(p_1 / p_2) 
p_icc
