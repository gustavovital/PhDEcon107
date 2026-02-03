library(tidyverse)
library(lubridate)

ECB_Data_Portal_long_20260203013253 <- read_delim("~/GitHub/PhDEcon107/data/ECB Data Portal long_20260203013253.csv", delim = ";", escape_double = FALSE, col_types = cols(OBS.VALUE = col_character()), trim_ws = TRUE)
EUEPUINDXM <- read_csv("~/GitHub/PhDEcon107/data/EUEPUINDXM.csv")
vstoxx <- read.csv("~/GitHub/PhDEcon107/data/vstoxx.txt", sep=";")

vstoxx <- vstoxx %>%
  mutate(Date = dmy(Date))

vstoxx <- vstoxx %>%
  dplyr::select(
    DATE = Date,
    vstoxx = Indexvalue
  )

ciss <- ECB_Data_Portal_long_20260203013253 %>%
  dplyr::select(
    DATE = DATE,
    CISS = OBS.VALUE
  )

# EPU <- EUEPUINDXM %>% 
#   dplyr::select(DATE = observation_date,
#                 EPU = EUEPUINDXM)


ciss <- ciss %>%
  mutate(
    DATE = as.Date(DATE),
    CISS = as.numeric(str_replace(CISS, ",", "."))
  )

ciss <- ciss %>%
  filter(!is.na(CISS)) %>% 
  mutate(DATE = floor_date(DATE, unit = "month")) %>%
  group_by(DATE) %>%
  summarise(
    CISS_m = mean(CISS, na.rm = TRUE)
  ) %>%
  ungroup()

vstoxx <- vstoxx %>%
  filter(!is.na(vstoxx)) %>% 
  mutate(DATE = floor_date(DATE, unit = "month")) %>%
  group_by(DATE) %>%
  summarise(
    vstoxx = mean(vstoxx, na.rm = TRUE)
  ) %>%
  ungroup()

vstoxx <- vstoxx %>% 
  filter(DATE < as.Date('2025-01-01') & DATE >= as.Date('2020-01-01'))

ciss <- ciss %>% 
  filter(DATE < as.Date('2025-01-01'))


# EPU <- EPU %>% 
#   filter(DATE < as.Date('2025-01-01'))
  
# Now the test
model_name <- "norm_yiyanghkust_lag_4" # smaller AIC BIC
ms <- models_list[[model_name]]

crisis_regime <- which.max(ms@std)
prob <- ms@Fit@smoProb[-1, crisis_regime]

msm_probs <- data.frame(
  DATE = data$DATE,
  p_crisis = prob
)

# create validation data ####
validation_data <- msm_probs %>%
  left_join(ciss, by = "DATE") %>%
  left_join(vstoxx, by = "DATE") %>%
  na.omit()

# test correlation ####
cor(validation_data$p_crisis, validation_data$CISS_m)
cor(validation_data$p_crisis, validation_data$vstoxx)

#check medias
validation_data <- validation_data %>%
  mutate(crisis = ifelse(p_crisis > 0.5, 1, 0))

t.test(CISS_m ~ crisis, data = validation_data)
t.test(vstoxx ~ crisis, data = validation_data)

# more regressions
summary(lm(p_crisis ~ CISS_m, data = validation_data))
summary(lm(p_crisis ~ vstoxx, data = validation_data))

# graph ####
validation_data <- validation_data %>%
  mutate(
    CISS_z   = as.numeric(scale(CISS_m)),
    vstoxx_z = as.numeric(scale(vstoxx))
  )

ggplot(validation_data, aes(x = DATE)) +
  geom_line(aes(y = p_crisis, color = "MSM crisis probability"), linewidth = 1) +
  geom_line(aes(y = CISS_z, color = "CISS (standardized)"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = vstoxx_z, color = "VSTOXX (standardized)"), linetype = "dotted", linewidth = 1) +
  scale_color_manual(
    values = c(
      "MSM crisis probability" = "black",
      "CISS (standardized)" = "red",
      "VSTOXX (standardized)" = "blue"
    )
  ) +
  labs(
    y = "Crisis probability / standardized indicators",
    x = "",
    color = "",
    title = "External validation of MSM regimes"
  ) +
  theme_minimal()

