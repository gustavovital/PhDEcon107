# Define variáveis e rolling_means
sent_all <- readRDS("C:/Users/gusta/Documents/GitHub/PhDEcon107/data/sent_all_new.rds")

data <- data2_new # %>% 
  # filter(DATE >= as.Date('2010-01-01'))

data <- sent_all %>% 
  filter(DATE >= as.Date('2010-01-01'))

sentiment_vars <- c("weighted_sentiment_finbert", "weighted_sentiment_yiyanghkust", "weighted_lm_sentiment")
rolling_means <- 2:6

models_list <- list()
models_list_p1 <- list()  # para modelos com p = 1

# Estima os dois tipos de modelos
for (sentiment_var in sentiment_vars) {
  for (lag in rolling_means) {
    
    var_name <- paste0(sentiment_var, "_", lag)
    formula_text <- paste0("icc ~ ", var_name)
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
    summary(ms_model_p0)
    # MSM com p = 1
    ms_model_p1 <- tryCatch({
      msmFit(lm_model, k = 2, p = 1, sw = c(TRUE, TRUE, TRUE, TRUE),
             control = list(parallel = FALSE, trace = FALSE))
    }, error = function(e) {
      message(paste("Error p=1 in:", model_name))
      return(NULL)
    })
    print('============= MODEL P1 ================')
    summary(ms_model_p1)
    
    models_list[[model_name]] <- ms_model_p0
    models_list_p1[[model_name]] <- ms_model_p1
  }
}



