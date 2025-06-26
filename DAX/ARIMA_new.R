rm(list = ls())
library(fpp3)
library(tseries)
library(zoo)
library(forecast)


setwd("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\DAX")
source("dax_procs.R")
source("Evaluation.R")
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
# acf(dat_returns$ret5)
# pacf(dat_returns$ret5)
# 
# qqnorm(dat_returns$ret3, main = "QQ-Plot der log returns")
# qqline(dat_returns$ret3, col = "red")
# Box.test(dat_returns$ret1)

n <- nrow(dat_returns)
n_train <- floor(0.7 * n)
n_val <- floor(0.1 * n) + n_train
arima_train_data <- dat_returns[1:n_train, ]
arima_val_data <- dat_returns[(n_train + 1):n_val, ]
arima_test_data <- dat_returns[(n_val + 1):n, ]
# 
#  model_r1 <- auto.arima(arima_train_data$ret1, seasonal = FALSE)
#  summary(model_r1) # ARIMA(1,0,0)
# model_r2 <- auto.arima(arima_train_data$ret2, seasonal = FALSE)
#  summary(model_r2) # ARIMA(2,0,0) #Experimentieren hat ARIMA(3,0,2) & (4,0,2) & (5,0,0)
#  model_r3 <- auto.arima(arima_train_data$ret3, seasonal = FALSE)
#  summary(model_r3) # ARIMA(5,0,1) ARIMA (4,0,1)
#  fit_1 <- arima(arima_train_data$ret3, order = c(4,0,1))
#  summary(fit_1)
#  model_r4 <- auto.arima(arima_train_data$ret4, seasonal = FALSE)
#  summary(model_r4) # ARIMA(5,0,0) ARIMA(5,0,3) oder MA 1 2
#  fit_1 <- arima(arima_train_data$ret4, order = c(5,0,3))
#  summary(fit_1)
#  model_r5 <- auto.arima(arima_train_data$ret5, seasonal = FALSE)
#  summary(model_r5) # ARIMA(5,0,1) ARIMA (6,0,3)



arima_forecasts <- function(dataset){
  
  # für h=1
  fit_1 <- arima(dataset$ret1, order = c(1,0,0))
  forc_1 <- forecast(fit_1, level = c(50, 95),  h=1, bootstrap = TRUE)
  forecasts_1 <- c(forc_1$lower[1, "95%"], forc_1$lower[1, "50%"], forc_1$mean[1], forc_1$upper[1, "50%"], forc_1$upper[1, "95%"])
  print(forecasts_1)
  # für h=2
  fit_2 <- arima(dataset$ret2, order = c(4,0,2))
  forc_2 <- forecast(fit_2, level = c(50, 95),  h=2, bootstrap = TRUE)
  forecasts_2<- c(forc_2$lower[2, "95%"], forc_2$lower[2, "50%"], forc_2$mean[2], forc_2$upper[2, "50%"], forc_2$upper[2, "95%"])
  print(forecasts_2)
  # für h=3
  fit_3 <- arima(dataset$ret3, order = c(4,0,1))
  forc_3 <- forecast(fit_3, level = c(50, 95),  h=3, bootstrap = TRUE)
  forecasts_3 <- c(forc_3$lower[3, "95%"], forc_3$lower[3, "50%"], forc_3$mean[3], forc_3$upper[3, "50%"], forc_3$upper[3, "95%"])
  print(forecasts_3)
  # für h=4
  fit_4 <- arima(dataset$ret4, order = c(5,0,3))
  forc_4 <- forecast(fit_4, level = c(50, 95),  h=4, bootstrap = TRUE)
  forecasts_4 <- c(forc_4$lower[4, "95%"], forc_4$lower[4, "50%"], forc_4$mean[4], forc_4$upper[4, "50%"], forc_4$upper[4, "95%"])
  print(forecasts_4)
  # für h=5
  fit_5 <- arima(dataset$ret5, order = c(6,0,3))
  forc_5 <- forecast(fit_5, level = c(50, 95),  h=5, bootstrap = TRUE)
  forecasts_5<- c(forc_5$lower[5, "95%"], forc_5$lower[5, "50%"], forc_5$mean[5], forc_5$upper[5, "50%"], forc_5$upper[5, "95%"])
  print(forecasts_5)
  predictions <- as.data.frame(rbind(forecasts_1, forecasts_2, forecasts_3, forecasts_4, forecasts_5))
  colnames(predictions) = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  rownames(predictions) = c("ret1", "ret2", "ret3", "ret4", "ret5")
  
  return(predictions)
}

arima_evaluation_matrix <- arima_val_data
arima_evaluation_matrix$QS <- NA

for(i in (nrow(arima_val_data)*0.48): (nrow(arima_val_data)-6)) {
  print (paste0("Fortschritt: ", i/ nrow(arima_val_data))) # Fortschrittsanzeige
  dataset <- bind_rows(arima_train_data, arima_val_data[1:i,])
  dataset <- tail(dataset, 365)
  forecasts_arima <- arima_forecasts(dataset)
  print(forecasts_arima)
  
  qs_score <- evaluation(datum= dataset$Datum[nrow(dataset)], data=arima_val_data, forecasts=forecasts_arima)
  print(qs_score)
  arima_evaluation_matrix$QS[i] <- qs_score
  
}     
print(mean(arima_evaluation_matrix$QS, na.rm = TRUE))
