
library(fpp3)
library(tseries)
library(zoo)
library(forecast)

set.seed(1)

setwd("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\DAX")
## Erstellschleifen der jeweiligen skripts müssen hierfür auskommentiert werden!!
source("dax_procs.R")
source("Evaluation.R")
source("Basic GARCH.R")
source("Quantil Regression.R")

tau <- c(.025, .25, .5, .75, .975) # quantile levels

# DAX Werte importieren und Returns berechnen -----------------------------

dax_data = get.hist.quote(instrument="^GDAXI", start="2010-01-01",
                          end="2024-02-14", quote="Adjusted",
                          origin="1970-01-01", provider="yahoo",
                          compression="d", retclass="zoo")
dax_data_filled <- na.locf(dax_data)
dax_data.df <- fortify.zoo(dax_data_filled)

dax_data.df <- rename(dax_data.df, "Datum" = "Index")
dax_data.df <- rename(dax_data.df, "Adj.Close" = "Adjusted")
dat_returns <- dax_data.df %>%
  mutate(ret1 = compute_return(Adj.Close, h = 1), 
         ret2 = compute_return(Adj.Close, h = 2),
         ret3 = compute_return(Adj.Close, h = 3),
         ret4 = compute_return(Adj.Close, h = 4),
         ret5 = compute_return(Adj.Close, h = 5))

dat_returns <- dat_returns[6:nrow(dat_returns), ]

dat_returns <- dat_returns %>% filter(format(Datum, "%Y") != "2020")


# n <- nrow(dat_returns)
dat_returns <- tail(dat_returns, nrow(dax_r5))
n_train <- floor(0.7 * n)
n_val <- floor(0.1 * n) + n_train
comb_train_data <- dat_returns[1:n_train, ]
comb_val_data <- dat_returns[(n_train + 1):n_val, ]
comb_test_data <- dat_returns[(n_val + 1):n, ]

# Forecasts erzeugen ------------------------------------------------------
method_combination <- function(dataset, datum){
  garch_for <- garch_forecasts(dataset)
  quantreg_r1 <- dax_r1_quantreg_forecasts (day = datum, data = dax_r1_test)
  quantreg_r2 <- dax_r2_quantreg_forecasts (day = datum, data = dax_r2_test)
  quantreg_r3 <- dax_r3_quantreg_forecasts (day = datum, data = dax_r3_test)
  quantreg_r4 <- dax_r4_quantreg_forecasts (day = datum, data = dax_r4_test)
  quantreg_r5 <- dax_r5_quantreg_forecasts (day = datum, data = dax_r5_test)
  quantreg_for <- rbind(quantreg_r1, quantreg_r2, quantreg_r3, quantreg_r4, quantreg_r5)
  #colnames(quantreg_for) <- c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  comb_forecast <- as.data.frame(matrix(nrow=5 , ncol = 5))
  for(i in 1:5){
    for(j in 1:5){
      comb_forecast[i, j] <- (0.2 * garch_for[i,j]) + (0.8*quantreg_for[i,j])
    }
  }
  colnames(comb_forecast) <- c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  return(comb_forecast)
}



# Test/ Val Score ---------------------------------------------------------

comb_evaluation_matrix <- comb_test_data
comb_evaluation_matrix$QS <- NA

for(i in 1: (nrow(comb_test_data) - 6)) {
  print (paste0("Fortschritt: ", i/ nrow(comb_test_data))) # Fortschrittsanzeige
  dataset <- bind_rows(comb_train_data, comb_test_data[1:i,])
  
  dataset <- tail(dataset, 365)
  forecasts_comb <- method_combination(dataset, as.Date(comb_test_data$Datum[i]))
  print(forecasts_comb)
  qs_score <- evaluation(datum= dataset$Datum[nrow(dataset)], data=comb_test_data, forecasts=forecasts_comb)
  print(qs_score)
  comb_evaluation_matrix$QS[i] <- qs_score
  
}    

print(mean(comb_evaluation_matrix$QS, na.rm = TRUE))
