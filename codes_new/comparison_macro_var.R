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

EPU <- EUEPUINDXM %>%
  dplyr::select(DATE = observation_date,
                EPU = EUEPUINDXM)


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

# filters ----

vstoxx <- vstoxx %>% 
  filter(DATE < as.Date('2026-01-01') & DATE >= as.Date('2010-01-01'))

ciss <- ciss %>% 
  filter(DATE < as.Date('2026-01-01')& DATE >= as.Date('2010-01-01'))


EPU <- EPU %>%
  filter(DATE < as.Date('2026-01-01')& DATE >= as.Date('2010-01-01'))

# Testing correlion ====
# model_name <- "norm_yiyanghkust_lag_4" # smaller AIC BIC
# ms <- models_list[[model_name]]
# 
# crisis_regime <- which.max(ms@std)
# prob <- ms@Fit@smoProb[-1, crisis_regime]
# 
# msm_probs <- data.frame(
#   DATE = data$DATE,
#   p_crisis = prob
# )
# 
# # create validation data ####
# validation_data <- msm_probs %>%
#   left_join(ciss, by = "DATE") %>%
#   left_join(vstoxx, by = "DATE") %>%
#   na.omit()
# 
# # test correlation ####
# cor(validation_data$p_crisis, validation_data$CISS_m)
# cor(validation_data$p_crisis, validation_data$vstoxx)
# 
# #check medias
# validation_data <- validation_data %>%
#   mutate(crisis = ifelse(p_crisis > 0.5, 1, 0))
# 
# t.test(CISS_m ~ crisis, data = validation_data)
# t.test(vstoxx ~ crisis, data = validation_data)
# 
# # more regressions
# summary(lm(p_crisis ~ CISS_m, data = validation_data))
# summary(lm(p_crisis ~ vstoxx, data = validation_data))
# 
# # graph ####
# validation_data <- validation_data %>%
#   mutate(
#     CISS_z   = as.numeric(scale(CISS_m)),
#     vstoxx_z = as.numeric(scale(vstoxx))
#   )
# 
# ggplot(validation_data, aes(x = DATE)) +
#   geom_line(aes(y = p_crisis, color = "MSM crisis probability"), linewidth = 1) +
#   geom_line(aes(y = CISS_z, color = "CISS (standardized)"), linetype = "dashed", linewidth = 1) +
#   geom_line(aes(y = vstoxx_z, color = "VSTOXX (standardized)"), linetype = "dotted", linewidth = 1) +
#   scale_color_manual(
#     values = c(
#       "MSM crisis probability" = "black",
#       "CISS (standardized)" = "red",
#       "VSTOXX (standardized)" = "blue"
#     )
#   ) +
#   labs(
#     y = "Crisis probability / standardized indicators",
#     x = "",
#     color = "",
#     title = "External validation of MSM regimes"
#   ) +
#   theme_minimal()
# 

validate_regime <- function(ms_model, data, icc_var = "icc",
                            vstoxx_var = "vstoxx",
                            epu_var = "epu",
                            ciss_var = "ciss") {
  
  # Extrair probabilidades suavizadas
  prob <- ms_model@Fit@smoProb[-1, ]
  
  if(ncol(prob) != 2){
    stop("Função válida apenas para modelo com 2 regimes.")
  }
  
  # Criar regimes dominantes
  data$regime1_prob <- prob[,1]
  data$regime2_prob <- prob[,2]
  
  data$regime_class <- ifelse(data$regime2_prob > 0.5, 2, 1)
  
  # Média do ICC por regime
  mean_icc <- aggregate(data[[icc_var]], 
                        by = list(data$regime_class), 
                        mean, na.rm = TRUE)
  
  colnames(mean_icc) <- c("Regime", "Mean_ICC")
  print("Média do ICC por regime:")
  print(mean_icc)
  
  # Identificar crise como regime com menor ICC
  crisis_regime <- mean_icc$Regime[which.min(mean_icc$Mean_ICC)]
  
  cat("\nRegime identificado como CRISE:", crisis_regime, "\n\n")
  
  # Criar probabilidade de crise corretamente
  if(crisis_regime == 1){
    data$prob_crisis <- data$regime1_prob
  } else {
    data$prob_crisis <- data$regime2_prob
  }
  
  data$crisis_dummy <- ifelse(data$prob_crisis > 0.5, 1, 0)
  
  # ---------------------------------
  # 1️⃣ Correlação simples
  # ---------------------------------
  
  cat("Correlação com VSTOXX:\n")
  print(cor.test(data$prob_crisis, vstoxx$vstoxx, 
                 use = "complete.obs"))
  
  cat("\nCorrelação com EPU:\n")
  print(cor.test(data$prob_crisis, EPU$EPU, 
                 use = "complete.obs"))
  
  cat("\nCorrelação com CISS:\n")
  print(cor.test(data$prob_crisis, ciss[["CISS_m"]], 
                 use = "complete.obs"))
  
  # ---------------------------------
  # 2️⃣ Diferença de médias
  # ---------------------------------
  
  cat("\nTeste de diferença de médias (VSTOXX):\n")
  print(t.test(vstoxx$vstoxx ~ data$crisis_dummy))
  
  cat("\nTeste de diferença de médias (EPU):\n")
  print(t.test(EPU$EPU ~ data$crisis_dummy))
  
  cat("\nTeste de diferença de médias (CISS):\n")
  print(t.test(ciss$CISS_m ~ data$crisis_dummy))
  # 
  # # ---------------------------------
  # # 3️⃣ Regressão auxiliar
  # # ---------------------------------
  # 
  cat("\nRegressão auxiliar VSTOXX:\n")
  print(summary(lm(vstoxx$vstoxx ~ data$prob_crisis)))
  
  cat("\nRegressão auxiliar EPU:\n")
  print(summary(lm(EPU$EPU ~ data$prob_crisis)))
  
  cat("\nRegressão auxiliar CISS:\n")
  print(summary(lm(ciss$CISS_m ~ data$prob_crisis)))
  
  # return(invisible(data))
}

validate_regime(models_list[["norm_finbert_lag_2"]], data)

# generalize models:

validate_all_models <- function(models_list, data,
                                icc_var = "icc",
                                vstoxx_var = "vstoxx",
                                epu_var = "epu",
                                ciss_var = "ciss") {
  
  results <- data.frame()
  
  for(model_name in names(models_list)) {
    
    cat("\n==============================\n")
    cat("Running model:", model_name, "\n")
    
    ms_model <- models_list[[model_name]]
    
    # Extrair probabilidades suavizadas
    prob <- ms_model@Fit@smoProb[-1, ]
    
    if(ncol(prob) != 2) next
    
    # Classificação de regimes
    data$regime1_prob <- prob[,1]
    data$regime2_prob <- prob[,2]
    
    data$regime_class <- ifelse(data$regime2_prob > 0.5, 2, 1)
    
    # Identificar regime crise
    mean_icc <- aggregate(data[[icc_var]],
                          by = list(data$regime_class),
                          mean, na.rm = TRUE)
    
    crisis_regime <- mean_icc$Group.1[which.min(mean_icc$x)]
    
    if(crisis_regime == 1){
      data$prob_crisis <- data$regime1_prob
    } else {
      data$prob_crisis <- data$regime2_prob
    }
    
    data$crisis_dummy <- ifelse(data$prob_crisis > 0.5, 1, 0)
    
    # Correlações
    cat("Correlação com VSTOXX:\n")
    print(cor.test(data$prob_crisis, vstoxx$vstoxx, use="complete.obs"))
    cat("Correlação com EPU:\n")
    print(cor.test(data$prob_crisis, EPU$EPU, use="complete.obs"))
    cat("Correlação com CISS:\n")
    print(cor.test(data$prob_crisis, ciss$CISS_m, use="complete.obs"))
    
    # Regressões
    cat("\nRegressão auxiliar vstoxx:\n")
    print(summary(lm(vstoxx$vstoxx ~ data$prob_crisis)))
    cat("\nRegressão auxiliar CISS:\n")
    print(summary(lm(ciss$CISS_m ~ data$prob_crisis)))
    cat("\nRegressão auxiliar EPU:\n")
    print(summary(lm(EPU$EPU ~ data$prob_crisis)))
    
    # Guardar resultados
    # results <- rbind(results, data.frame(
    #   model = model_name,
    #   crisis_regime = crisis_regime,
    #   cor_vstoxx = cor_v$estimate,
    #   p_vstoxx = cor_v$p.value,
    #   cor_epu = cor_e$estimate,
    #   p_epu = cor_e$p.value,
    #   cor_ciss = cor_c$estimate,
    #   p_ciss = cor_c$p.value,
    #   R2_vstoxx = reg_v$r.squared,
    #   R2_ciss = reg_c$r.squared,
    #   R2_epu = reg_e$r.squared
    # ))
  }
  
  return(results)
}

validate_all_models <- function(models_list, data,
                                icc_var = "icc",
                                vstoxx_var = "vstoxx",
                                epu_var = "EPU",
                                ciss_var = "CISS_m",
                                standardize = TRUE) {
  
  results <- data.frame()
  
  # ----------------------------
  # Helper: z-score with NA-safe behavior
  # ----------------------------
  zscore <- function(x) {
    x <- as.numeric(x)
    mu <- mean(x, na.rm = TRUE)
    sdv <- stats::sd(x, na.rm = TRUE)
    if (is.na(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
    (x - mu) / sdv
  }
  
  # ----------------------------
  # Optional standardizations (in-place)
  # - Standardize ICC and the external series (vstoxx, EPU, CISS)
  # - Use the current sample (the vectors you pass in) for mean/sd
  # ----------------------------
  if (standardize) {
    if (!icc_var %in% names(data)) stop("icc_var not found in 'data'.")
    
    data$icc <- zscore(data$icc)
    
    # standardize the externals if they exist
    if (exists("vstoxx") && vstoxx_var %in% names(vstoxx)) {
      vstoxx$vstoxx <- zscore(vstoxx$vstoxx)
    }
    if (exists("EPU") && epu_var %in% names(EPU)) {
      EPU$EPU <- zscore(EPU$EPU)
    }
    if (exists("CISS_m") && ciss_var %in% names(ciss)) {
      ciss$CISS_m <- zscore(ciss$CISS_m)
    }
  }
  
  for (model_name in names(models_list)) {
    
    cat("\n==============================\n")
    cat("Running model:", model_name, "\n")
    
    ms_model <- models_list[[model_name]]
    
    # Extract smoothed probabilities
    prob <- ms_model@Fit@smoProb[-1, , drop = FALSE]
    if (ncol(prob) != 2) next
    
    # Regime classification
    data$regime1_prob <- prob[, 1]
    data$regime2_prob <- prob[, 2]
    data$regime_class <- ifelse(data$regime2_prob > 0.5, 2, 1)
    
    # Identify crisis regime: the one with lower mean ICC
    mean_icc <- aggregate(data[[icc_var]],
                          by = list(data$regime_class),
                          mean, na.rm = TRUE)
    crisis_regime <- mean_icc$Group.1[which.min(mean_icc$x)]
    
    data$prob_crisis <- if (crisis_regime == 1) data$regime1_prob else data$regime2_prob
    data$crisis_dummy <- ifelse(data$prob_crisis > 0.5, 1, 0)
    
    # Correlations (use complete.obs explicitly)
    cat("Correlação com VSTOXX:\n")
    print(cor.test(data$prob_crisis, vstoxx$vstoxx, use = "complete.obs"))
    
    cat("Correlação com EPU:\n")
    print(cor.test(data$prob_crisis, EPU$EPU, use = "complete.obs"))
    
    cat("Correlação com CISS:\n")
    print(cor.test(data$prob_crisis, ciss$CISS_m, use = "complete.obs"))
    
    # Auxiliary regressions
    cat("\nRegressão auxiliar VSTOXX:\n")
    # print(sd(ciss$CISS_m))
    # print(mean(ciss$CISS_m))
    print(summary(lm(vstoxx$vstoxx ~ data$prob_crisis)))
    
    cat("\nRegressão auxiliar CISS:\n")
    print(summary(lm(ciss$CISS_m ~ data$prob_crisis)))
    
    cat("\nRegressão auxiliar EPU:\n")
    print(summary(lm(EPU$EPU ~ data$prob_crisis)))
  }
  
  return(results)
}

# Run
validate_all_models(models_list, data, standardize = TRUE)


validate_all_models(models_list, data)
