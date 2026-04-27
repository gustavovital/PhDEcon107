library(MSwM)

print_ll_aic_bic <- function(data,
                             sentiment_vars,
                             rolling_means,
                             p = 1) {
  
  results <- data.frame(
    sentiment = character(),
    lag = integer(),
    k = integer(),
    logLik = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    converged = logical(),
    stringsAsFactors = FALSE
  )
  
  for (sent_var in sentiment_vars) {
    for (lag in rolling_means) {
      
      var  <- paste0(sent_var, "_", lag)
      form <- as.formula(paste0("icc ~ ", var))
      
      # -----------------------
      # k = 1 (linear)
      # -----------------------
      lm_k1 <- lm(form, data = data, na.action = na.exclude)
      
      n     <- nobs(lm_k1)
      pcoef <- length(coef(lm_k1))
      ll1   <- as.numeric(logLik(lm_k1))
      
      aic1 <- -2 * ll1 + 2 * pcoef
      bic1 <- -2 * ll1 + log(n) * pcoef
      
      results <- rbind(results, data.frame(
        sentiment = sent_var,
        lag = lag,
        k = 1,
        logLik = ll1,
        AIC = aic1,
        BIC = bic1,
        converged = TRUE
      ))
      
      # -----------------------
      # k = 2 e 3 (MSM)
      # -----------------------
      for (k in 2:3) {
        
        ms <- tryCatch({
          msmFit(
            lm_k1,
            k = k,
            p = p,
            sw = c(TRUE, TRUE, TRUE, TRUE),
            control = list(parallel = FALSE, trace = FALSE)
          )
        }, error = function(e) NULL)
        
        if (is.null(ms)) {
          
          results <- rbind(results, data.frame(
            sentiment = sent_var,
            lag = lag,
            k = k,
            logLik = NA,
            AIC = NA,
            BIC = NA,
            converged = FALSE
          ))
          
        } else {
          
          ll <- ms@Fit@logLikel
          
          # número de parâmetros livres
          npar <- k * pcoef + k + k * (k - 1)
          
          aic <- -2 * ll + 2 * npar
          bic <- -2 * ll + log(n) * npar
          
          results <- rbind(results, data.frame(
            sentiment = sent_var,
            lag = lag,
            k = k,
            logLik = ll,
            AIC = aic,
            BIC = bic,
            converged = TRUE
          ))
        }
      }
      
      # impressão amigável por bloco
      cat("\n==============================\n")
      cat("Sentiment:", sent_var, " | Lag:", lag, "\n")
      print(results[results$sentiment == sent_var &
                      results$lag == lag, ])
    }
  }
  
  return(results)
}

ic_results <- print_ll_aic_bic(
  data = data,
  sentiment_vars = sentiment_vars,
  rolling_means = rolling_means,
  p = 1
)
