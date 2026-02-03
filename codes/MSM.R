# Define variáveis e rolling_means
data <- readRDS("C:/Users/gusta/Documents/GitHub/PhDEcon107/data/sent_all.rds")

sentiment_vars <- c("norm_finbert", "norm_yiyanghkust", "norm_lm_sentiment")
rolling_means <- 2:6

models_list <- list()
models_list_p1 <- list()  # para modelos com p = 1

# Estima os dois tipos de modelos
for (sentiment_var in sentiment_vars) {
  for (lag in rolling_means) {
    
    var_name <- paste0(sentiment_var, "_", lag)
    formula_text <- paste0("ICC ~ ", var_name)
    formula_model <- as.formula(formula_text)
    
    lm_model <- lm(formula_model, data = data, na.action = na.exclude)
    model_name <- paste(sentiment_var, "lag", lag, sep = "_")
    
    # MSM com p = 0
    ms_model_p0 <- tryCatch({
      msmFit(lm_model, k = 2, p = 0, sw = c(TRUE, TRUE, TRUE),
             control = list(parallel = FALSE, trace = FALSE))
    }, error = function(e) {
      message(paste("Error p=0 in:", model_name))
      return(NULL)
    })
    print(paste('Formula: ', formula_model))
    print('============= MODEL P0 ================')
    print(ms_model_p0)
    # MSM com p = 1
    ms_model_p1 <- tryCatch({
      msmFit(lm_model, k = 2, p = 1, sw = c(TRUE, TRUE, TRUE, TRUE),
             control = list(parallel = FALSE, trace = FALSE))
    }, error = function(e) {
      message(paste("Error p=1 in:", model_name))
      return(NULL)
    })
    print('============= MODEL P1 ================')
    print(ms_model_p1)
    
    models_list[[model_name]] <- ms_model_p0
    models_list_p1[[model_name]] <- ms_model_p1
  }
}



# Export to Latex (NOT USED) ====

# # Tabela com coeficientes formatados
# coef_table <- data.frame(
#   model = character(),
#   coef_regime1_p0 = character(),
#   coef_regime2_p0 = character(),
#   coef_regime1_p1 = character(),
#   coef_regime2_p1 = character(),
#   stringsAsFactors = FALSE
# )
# 
# # Função auxiliar para extrair coeficiente formatado
# extract_coef <- function(model, col_index = 2) {
#   if (is.null(model)) return(c(NA, NA))
#   
#   coefs <- model@Coef
#   ses <- model@seCoef
#   out <- c()
#   
#   for (r in 1:2) {
#     coef_val <- coefs[r, col_index]
#     se_val <- ses[[r]][col_index]
#     t_stat <- coef_val / se_val
#     p_val <- 2 * (1 - pnorm(abs(t_stat)))
#     
#     stars <- if (is.na(p_val)) {
#       ""
#     } else if (p_val < 0.001) {
#       "***"
#     } else if (p_val < 0.01) {
#       "**"
#     } else if (p_val < 0.05) {
#       "*"
#     } else {
#       ""
#     }
#     
#     out[r] <- paste0(round(coef_val, 3), stars)
#   }
#   return(out)
# }
# 
# # Preenche a tabela
# for (name in names(models_list)) {
#   coef_p0 <- extract_coef(models_list[[name]])
#   coef_p1 <- extract_coef(models_list_p1[[name]])
#   
#   coef_table <- rbind(
#     coef_table,
#     data.frame(
#       model = name,
#       coef_regime1_p0 = coef_p0[1],
#       coef_regime2_p0 = coef_p0[2],
#       coef_regime1_p1 = coef_p1[1],
#       coef_regime2_p1 = coef_p1[2],
#       stringsAsFactors = FALSE
#     )
#   )
# }
# 
# latex_table <- xtable(coef_table, align = "llcccc", caption = "Markov Switching Regression Estimates with and without Autoregressive Term")
# print(latex_table, include.rownames = FALSE)
