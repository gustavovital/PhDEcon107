library(dplyr)

data <- data %>% 
  filter(DATE >= as.Date('2010-01-01'))

data_2 <- data %>% 
  filter(DATE >= as.Date('2010-01-01'))

results_sd_effect <- data.frame()

for (sentiment_var in sentiment_vars) {
  for (k in rolling_means) {
    
    var_name <- paste0(sentiment_var, "_", k)
    model_name <- paste(sentiment_var, "lag", k, sep = "_")
    
    ms_model <- models_list[[model_name]]
    
    if (is.null(ms_model)) next
    
    # -------------------------------
    # 1️⃣ Extrair betas por regime
    # -------------------------------
    coefs <- ms_model@Coef
    
    # localizar linha correta da variável
    beta_r1 <- coefs[var_name][1, 1]
    beta_r2 <- coefs[var_name][2, 1]
    
    # -------------------------------
    # 2️⃣ Calcular sd na amostra usada
    # -------------------------------
    model_data <- na.omit(data[, c("ICC", var_name)])
    sd_var <- sd(model_data[[var_name]])
    
    # -------------------------------
    # 3️⃣ Calcular efeito 1 sd
    # -------------------------------
    effect_r1 <- beta_r1 * sd_var
    effect_r2 <- beta_r2 * sd_var
    
    # -------------------------------
    # 4️⃣ Armazenar
    # -------------------------------
    results_sd_effect <- rbind(
      results_sd_effect,
      data.frame(
        variable = var_name,
        beta_r1 = beta_r1,
        beta_r2 = beta_r2,
        sd_var = sd_var,
        effect_1sd_r1 = effect_r1,
        effect_1sd_r2 = effect_r2
      )
    )
  }
}

print(results_sd_effect)

results_sd_effect_p1 <- data.frame()

for (sentiment_var in sentiment_vars) {
  for (k in rolling_means) {
    
    var_name <- paste0(sentiment_var, "_", k)
    model_name <- paste(sentiment_var, "lag", k, sep = "_")
    
    ms_model <- models_list_p1[[model_name]]   # <-- aqui muda
    
    if (is.null(ms_model)) next
    
    # -------------------------------
    # 1️⃣ Extrair betas por regime
    # -------------------------------
    coefs <- ms_model@Coef
    
    # IMPORTANTE:
    # em p=1 ainda pegamos apenas o coeficiente da variável de sentimento
    beta_r1 <- coefs[var_name][1, 1]
    beta_r2 <- coefs[var_name][2, 1]
    
    # -------------------------------
    # 2️⃣ Calcular sd na amostra usada
    # -------------------------------
    model_data <- na.omit(data[, c("ICC", var_name)])
    sd_var <- sd(model_data[[var_name]])
    
    # -------------------------------
    # 3️⃣ Calcular efeito 1 sd
    # -------------------------------
    effect_r1 <- beta_r1 * sd_var
    effect_r2 <- beta_r2 * sd_var
    
    # -------------------------------
    # 4️⃣ Armazenar
    # -------------------------------
    results_sd_effect_p1 <- rbind(
      results_sd_effect_p1,
      data.frame(
        variable = var_name,
        beta_r1 = beta_r1,
        beta_r2 = beta_r2,
        sd_var = sd_var,
        effect_1sd_r1 = effect_r1,
        effect_1sd_r2 = effect_r2
      )
    )
  }
}

print(results_sd_effect_p1)


# estimação com variaveis padronizadas:

data_2 <- data

for (sentiment_var in sentiment_vars) {
  for (k in rolling_means) {
    
    var_name <- paste0(sentiment_var, "_", k)
    std_name <- paste0(var_name, "_std")
    
    data_2[[std_name]] <- as.numeric(scale(data_2[[var_name]]))
  }
}

library(MSwM)
library(dplyr)

results_ms <- data.frame()

models_list <- list()
models_list_p1 <- list()

for (sentiment_var in sentiment_vars) {
  for (lag in rolling_means) {
    
    var_name <- paste0(sentiment_var, "_", lag, "_std")
    formula_text <- paste0("ICC ~ ", var_name)
    formula_model <- as.formula(formula_text)
    
    lm_model <- lm(formula_model, data = data_2, na.action = na.exclude)
    model_name <- paste(sentiment_var, "lag", lag, "std", sep = "_")
    
    # MSM com p = 0
    ms_model_p0 <- tryCatch({
      msmFit(lm_model, k = 2, p = 0,
             sw = c(TRUE, TRUE, TRUE),
             control = list(parallel = FALSE, trace = FALSE))
    }, error = function(e) {
      message(paste("Error p=0 in:", model_name))
      return(NULL)
    })
    
    print(paste("Formula:", formula_model))
    print("============= MODEL P0 ================")
    summary(ms_model_p0)

    # MSM com p = 1
    ms_model_p1 <- tryCatch({
      msmFit(lm_model, k = 2, p = 1,
             sw = c(TRUE, TRUE, TRUE, TRUE),
             control = list(parallel = FALSE, trace = FALSE))
    }, error = function(e) {
      message(paste("Error p=1 in:", model_name))
      return(NULL)
    })
    
    # print("============= MODEL P1 ================")
    # summary(ms_model_p1)
    
    models_list[[model_name]] <- ms_model_p0
    models_list_p1[[model_name]] <- ms_model_p1
  }
}
