library(tidyverse)
library(tidytext)
library(readr)
library(ggplot2)

# get corpus clean
ecb_speech_corpus_clean <- read_csv("~/GitHub/PhDEcon107/data/ecb_speech_corpus_clean.csv")

# get LM 
lm_dict <- get_sentiments("loughran")

# get tokens (heavy df)
ecb_tokens <- ecb_speech_corpus_clean %>%
  mutate(DATE = as.Date(DATE)) %>%
  unnest_tokens(word, TEXT)

# count words
lm_scores_doc <- ecb_tokens %>%
  inner_join(lm_dict, by = "word") %>%
  count(ID, DATE, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0)

#### get weights ####
lm_scores_doc <- lm_scores_doc %>%
  mutate(
    total = positive + negative,
    lm_sentiment = (positive - negative) / (total + 1)
  )

doc_length <- ecb_tokens %>%
  count(ID, name = "tokens")

lm_scores_doc <- lm_scores_doc %>%
  left_join(doc_length, by = "ID")

#### Agreggate ####
lm_monthly_simple <- lm_scores_doc %>%
  mutate(MONTH = floor_date(DATE, "month")) %>%
  group_by(MONTH) %>%
  summarise(
    lm_sent_simple = mean(lm_sentiment, na.rm = TRUE)
  )

#### w weights ####
lm_monthly_weighted <- lm_scores_doc %>%
  mutate(MONTH = floor_date(DATE, "month")) %>%
  group_by(MONTH) %>%
  summarise(
    lm_sent_weighted =
      sum(lm_sentiment * tokens, na.rm = TRUE) /
      sum(tokens, na.rm = TRUE)
  )

## add to data ####
lm_monthly <- lm_scores_doc %>%
  mutate(DATE = floor_date(DATE, unit = "month")) %>%
  group_by(DATE) %>%
  summarise(
    weighted_lm_sentiment =
      sum(lm_sentiment * tokens, na.rm = TRUE) / sum(tokens, na.rm = TRUE),
    total_tokens = sum(tokens, na.rm = TRUE),
    n_docs = n()
  ) %>%
  ungroup()

lm_monthly <- lm_monthly %>%
  mutate(
    norm_lm_sentiment =
      (weighted_lm_sentiment - min(weighted_lm_sentiment, na.rm = TRUE)) /
      (max(weighted_lm_sentiment, na.rm = TRUE) -
         min(weighted_lm_sentiment, na.rm = TRUE))
  )

## moving average ####
for (i in 2:6) {
  
  col_raw  <- paste0("weighted_lm_sentiment_", i)
  col_norm <- paste0("norm_lm_sentiment_", i)
  
  lm_monthly[[col_raw]]  <-
    zoo::rollmean(lm_monthly$weighted_lm_sentiment,
                  k = i, fill = NA, align = "right")
  
  lm_monthly[[col_norm]] <-
    zoo::rollmean(lm_monthly$norm_lm_sentiment,
                  k = i, fill = NA, align = "right")
}


lm_monthly2 <- lm_monthly %>%
  filter(DATE < as.Date("2025-01-01"),
         DATE >= as.Date("2020-01-01")) %>%
  mutate(
    covid = ifelse(DATE < as.Date("2021-07-01") &
                     DATE > as.Date("2020-03-01"), 1, 0)
  ) %>%
  na.omit()

## join df ####
data2 <- readRDS("C:/Users/gusta/Documents/GitHub/PhDEcon107/data/data2.rds")

sent_all <- data2 %>%
  left_join(lm_monthly2, by = "DATE")

# save ####
saveRDS(
  sent_all,
  "C:/Users/gusta/Documents/GitHub/PhDEcon107/data/sent_all.rds"
)

## Comparison ####
sent_comp <- sent_all %>%
  dplyr::select(
    DATE,
    matches("^norm_(finbert|yiyanghkust|lm_sentiment)$"),
    matches("^norm_(finbert|yiyanghkust|lm_sentiment)_[2-6]$")
  ) %>%
  na.omit()


sent_comp <- na.omit(sent_comp)

cor_mat <- cor(
  sent_comp %>% dplyr::select(-DATE),
  use = "pairwise.complete.obs"
)

cor_df <- as.data.frame(cor_mat) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr")


cor_df <- cor_df %>%
  mutate(
    var1 = factor(var1, levels = colnames(cor_mat)),
    var2 = factor(var2, levels = colnames(cor_mat))
  ) %>%
  filter(as.numeric(var1) <= as.numeric(var2))  # triângulo inferior

ggplot(cor_df, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#b2182b",
    mid = "white",
    high = "#2166ac",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  geom_text(aes(label = round(corr, 2)),
            size = 2.6, color = "black") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Correlation between Transformer-Based and Lexicon-Based Sentiment Measures",
    subtitle = "Levels and rolling windows (2–6 months), weighted and normalized"
  )
