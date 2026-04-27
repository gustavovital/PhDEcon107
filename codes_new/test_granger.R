library(vars)
library(dplyr)

# Seleciona todas variáveis que começam com "norm_"
sentiment_vars_granger <- names(data)[grepl("^norm_", names(data))]

# Remover icc se por acaso estivesse (não está, mas segurança)
sentiment_vars_granger <- setdiff(sentiment_vars_granger, "icc")

sentiment_vars_granger <- c(sentiment_vars_granger)
print(sentiment_vars_granger)

run_granger_test <- function(data, sentiment_var, icc_var = "icc", max_lag = 4) {
  
  # Selecionar apenas ICC e a variável de sentimento
  df <- data[, c(icc_var, sentiment_var)]
  df <- na.omit(df)
  
  colnames(df) <- c("icc", "sentiment")
  
  # Checar tamanho mínimo
  if(nrow(df) < 30) return(NULL)
  
  # Seleção automática de lag via AIC
  lag_sel <- VARselect(df, lag.max = max_lag, type = "const")
  optimal_lag <- lag_sel$selection["AIC(n)"]
  
  # Garantir que lag não seja NA
  if(is.na(optimal_lag)) optimal_lag <- 1
  
  # Estimar VAR
  var_model <- VAR(df, p = optimal_lag, type = "const")
  
  # Testes Granger
  test_sent_to_icc <- causality(var_model, cause = "sentiment")
  test_icc_to_sent <- causality(var_model, cause = "icc")
  
  return(data.frame(
    variable = sentiment_var,
    lag_used = optimal_lag,
    p_sent_to_icc = test_sent_to_icc$Granger$p.value,
    p_icc_to_sent = test_icc_to_sent$Granger$p.value
  ))
}

results_granger <- lapply(sentiment_vars_granger, function(var) {
  run_granger_test(data, var)
})

results_granger <- do.call(rbind, results_granger)

print(results_granger)
