library(dplyr)
library(lubridate)
library(tseries)
library(zoo)

setwd("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\DAX")
source("dax_procs.R")
source("Evaluation.R")
tau <- c(.025, .25, .5, .75, .975) # quantile levels


dax_data = get.hist.quote(instrument="^GDAXI", start="2010-01-01",
                          end="2024-02-14", quote="Adjusted",
                          origin="1970-01-01", provider="yahoo",
                          compression="d", retclass="zoo")
dax_data_filled <- na.locf(dax_data)
dax_data.df <- fortify.zoo(dax_data_filled)
dax_data.df$Index <- as.Date(dax_data.df$Index)
dax_data.df <- rename(dax_data.df, "Datum" = "Index")
dax_data.df <- rename(dax_data.df, "Adj.Close" = "Adjusted")
dat_returns <- dax_data.df %>%
  mutate(ret1 = compute_return(Adj.Close, h = 1), 
         ret2 = compute_return(Adj.Close, h = 2),
         ret3 = compute_return(Adj.Close, h = 3),
         ret4 = compute_return(Adj.Close, h = 4),
         ret5 = compute_return(Adj.Close, h = 5))
dat_returns <- dat_returns %>% filter(format(Datum, "%Y") != "2020")
n <- nrow(dat_returns)
n_train <- floor(0.7 * n)
n_val <- floor(0.1 * n) + n_train
baseline_train_data <- dat_returns[1:n_train, ]
baseline_val_data <- dat_returns[(n_train + 1):n_val, ]
baseline_test_data <- dat_returns[(n_val + 1):n, ]

baseline_test <- tail(dat_returns, 0.2*nrow(dat_returns))

# Methode generiert Forecasts für alle Horizonte
dax_baseline_forecasts <- function(forecast_date){

 
returns <- baseline_val_data
returns <- returns %>% filter(Datum <= forecast_date)

print(returns)

pred_baseline <- matrix(NA, nrow = length(tau), ncol = 5)
for (jj in 1:5){
  tmp <- returns[, paste0("ret", jj)] %>% na.omit %>% tail(1000)
  pred_baseline[,jj] <- quantile(tmp, probs = tau)
}
pred_baseline.df <- data.frame(forecast_date = forecast_date, 
                      target = "DAX", horizon = paste(c(1, 2, 5:7), "day"),
                      q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                      q0.975 = NA)
pred_baseline.df[,4:8] <- t(pred_baseline)

return(pred_baseline.df)
}


# Compute Quantile Score  --------------------------------------------------


baseline_val_data$QS <- NA
for(i in 1: nrow(baseline_val_data) ){
  baseline_forecasts <- dax_baseline_forecasts(baseline_val_data$Datum[i])
  print(baseline_val_data$Datum[i])
  print(baseline_forecasts)
  baseline_val_data$QS[i] <- evaluation(baseline_val_data$Datum[i], baseline_val_data, baseline_forecasts) 
  print(baseline_val_data$QS[i])
}
tail(baseline_val_data)
print(baseline_qs <- mean(baseline_val_data$QS, na.rm = TRUE))
# dat_baseline <- dax_data.df %>%
#   mutate(ret1 = compute_return(Adj.Close, h = 1), 
#          ret2 = compute_return(Adj.Close, h = 2),
#          ret3 = compute_return(Adj.Close, h = 3),
#          ret4 = compute_return(Adj.Close, h = 4),
#          ret5 = compute_return(Adj.Close, h = 5))

# pred_baseline <- matrix(NA, nrow = length(tau), ncol = 5) 
# pred_baseline[,1] <- quantile(dat_baseline$ret1, probs = tau, na.rm = TRUE)
# pred_baseline[,2] <- quantile(dat_baseline$ret2, probs = tau, na.rm = TRUE)
# pred_baseline[,3] <- quantile(dat_baseline$ret3, probs = tau, na.rm = TRUE)
# pred_baseline[,4] <- quantile(dat_baseline$ret4, probs = tau, na.rm = TRUE)
# pred_baseline[,5] <- quantile(dat_baseline$ret5, probs = tau, na.rm = TRUE)
