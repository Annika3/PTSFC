library(dplyr)
library(lubridate)
library(tseries)
library(zoo)
library(fpp3)


setwd("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\DAX")
source("dax_procs.R")
source("Evaluation.R")
tau <- c(.025, .25, .5, .75, .975) # quantile levels


# DAX Werte importieren und Returns berechnen -----------------------------

dax_data = get.hist.quote(instrument="^GDAXI", start="2000-01-01",
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

ret.tsibble <- as_tsibble(dat_returns[6:nrow(dat_returns), ], index = Datum) # TS Objekt erstellen


# Dealing with holidays ---------------------------------------------------

ret.tsibble <- ret.tsibble %>% mutate(no_workday = ret1 == 0000)
ret.tsibble <- ret.tsibble %>% fill_gaps()
ret.tsibble <- ret.tsibble %>% mutate(no_workday = if_else(is.na(ret1), TRUE, no_workday))
ret.tsibble <- ret.tsibble %>% fill(everything(), .direction = "down")

# Unterteilung in Train, Validation und Test ------------------------------

n <- nrow(ret.tsibble)
n_train <- floor(0.7 * n)
n_val <- floor(0.1 * n) + n_train
arima_train_data <- ret.tsibble[1:n_train, ]
arima_val_data <- ret.tsibble[(n_train + 1):n_val, ]
arima_test_data <- ret.tsibble[(n_val + 1):n, ]

# Modelle mit Trainingssatz erstellen -------------------------------------------------------

'model_ret1 <- arima_train_data %>% fill_gaps() %>%  model(ARIMA(ret1 ~ no_workday))
report(model_ret1) #Model: LM w/ ARIMA(2,0,2)(1,0,0)[7] errors

model_ret2 <- arima_train_data %>% fill_gaps() %>%  model(ARIMA(ret2 ~ no_workday))
report(model_ret2) #LM w/ ARIMA(1,0,3)(1,0,1)[7] errors

model_ret3 <- arima_train_data %>% fill_gaps() %>%  model(ARIMA(ret3 ~ no_workday))
report(model_ret3) #LM w/ ARIMA(0,0,4)(2,0,0)[7] errors

model_ret4 <- arima_train_data %>% fill_gaps() %>%  model(ARIMA(ret4 ~ no_workday))
report(model_ret4) #LM w/ ARIMA(3,0,3) errors 

model_ret5 <- arima_train_data %>% fill_gaps() %>% model(ARIMA(ret5 ~ no_workday))
report(model_ret5) #LM w/ ARIMA(1,0,1)(1,0,2)[7] errors '


# Mit Rolling Window Forecasts erstellen ----------------------------------

arima_forecasts <- function(dataset){
  datum <- as.Date(dataset$Datum[nrow(dataset)]) #letztes Datum des "Fit" Datasets bestimmen
  i <- which(arima_test_data$Datum == datum)
  
   # für h=1
  fit_1 <- dataset %>% model(ARIMA(ret1 ~ 0+ no_workday + pdq(2, 0, 2) + PDQ(1,0,0)))
  if (any(is.na(residuals(fit_1)$.resid))) {
    return(NA)
  } else {
  for_1_boot <- forecast(fit_1, new_data = arima_test_data[i + 1,], bootstrap=TRUE, times = 1000)
  bootstrap_1  <- distributional::parameters(for_1_boot$ret1[1])
  bootstrap_values_1 <- bootstrap_1$x[[1]]
  forecasts_1 <- quantile(bootstrap_values_1, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  }
  # für h=2 
  fit_2 <- dataset %>% model(fable::ARIMA (ret2 ~ 0+ no_workday + pdq(1, 0, 3) + PDQ(1,0,1))) 
  if (any(is.na(residuals(fit_2)$.resid))) {
    return(NA)
  } else {
  for_2_boot <- forecast(fit_2, new_data = bind_rows(arima_test_data[i + 1,], arima_test_data[i + 2,]), bootstrap=TRUE, times = 1000)
  bootstrap_2  <- distributional::parameters(for_2_boot$ret2[2])
  bootstrap_values_2 <- bootstrap_2$x[[1]]
  forecasts_2 <- quantile(bootstrap_values_2, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  }
  # für h=3 
  fit_3 <- dataset %>% fill_gaps() %>% model(fable::ARIMA( ret3 ~ 0+ no_workday + pdq(0, 0, 4) + PDQ(2,0,0)))
  if (any(is.na(residuals(fit_3)$.resid))) {
    return(NA)
  } else {
  for_3_boot <- forecast(fit_3, new_data = bind_rows(arima_test_data[i + 1,], arima_test_data[i + 2,], arima_test_data[i + 3,]), bootstrap=TRUE, times = 1000)
  bootstrap_3  <- distributional::parameters(for_3_boot$ret3[3])
  bootstrap_values_3 <- bootstrap_3$x[[1]]
  forecasts_3 <- quantile(bootstrap_values_3, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  }
  # für h=4 
  fit_4 <- dataset %>% fill_gaps() %>% model(fable::ARIMA( ret4 ~ 0+ no_workday + pdq(3, 0, 3)+ PDQ(0,0,0)))
  if (any(is.na(residuals(fit_4)$.resid))) {
    return(NA)
  } else {
  for_4_boot <- forecast(fit_4, new_data = bind_rows(arima_test_data[i + 1,], arima_test_data[i + 2,], arima_test_data[i + 3,], arima_test_data[i + 4,]), bootstrap=TRUE, times = 1000)
  bootstrap_4  <- distributional::parameters(for_4_boot$ret4[4])
  bootstrap_values_4 <- bootstrap_4$x[[1]]
  forecasts_4 <- quantile(bootstrap_values_4, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  }
  # für h=5
  fit_5 <- dataset %>% fill_gaps() %>% model(fable::ARIMA( ret5 ~ 0+  no_workday + pdq(1, 0, 1) + PDQ(1,0,2)))
  if (any(is.na(residuals(fit_5)$.resid))) {
    return(NA)
  } else {
  # model_resid <- residuals(fit_5)
  # print(model_resid)
  for_5_boot <- forecast(fit_5, new_data = bind_rows(arima_test_data[i + 1,], arima_test_data[i + 2,], arima_test_data[i + 3,], arima_test_data[i + 4,], arima_test_data[i + 5,]), bootstrap=TRUE, times = 1000)
  bootstrap_5  <- distributional::parameters(for_5_boot$ret5[5])
  bootstrap_values_5 <- bootstrap_5$x[[1]]
  forecasts_5 <- quantile(bootstrap_values_5, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  }
  predictions <- as.data.frame(rbind(forecasts_1, forecasts_2, forecasts_3, forecasts_4, forecasts_5))
  colnames(predictions) = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  rownames(predictions) = c("ret1", "ret2", "ret3", "ret4", "ret5")
  
  return(predictions)
}
nrow(arima_test_data)
arima_evaluation_matrix <- arima_test_data
arima_evaluation_matrix$QS <- NA

 for(i in 1: (nrow(arima_test_data) - 6)) {
   print (paste0("Fortschritt: ", i/ nrow(arima_test_data))) # Fortschrittsanzeige
   dataset <- bind_rows(arima_val_data, arima_test_data[1:i,])
   dataset <- tail(dataset, 1000)
   forecasts_arima <- arima_forecasts(dataset)
   print(forecasts_arima)
   if (!any(is.na(forecasts_arima))) {
     qs_score <- evaluation(datum= dataset$Datum[nrow(dataset)], data=arima_test_data, forecasts=forecasts_arima)
     print(qs_score)
     arima_evaluation_matrix$QS[i] <- qs_score
   }
   
 }     

(mean(arima_evaluation_matrix$QS, na.rm = TRUE))






# Archiv ------------------------------------------------------------------

# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\DAX_Lags_h1.pdf")
# ret.tsibble %>% gg_lag ( ret1, geom = "point")
# dev.off()

# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\DAX_ret1.pdf", width = 7, height = 4)
# print (ggplot(dummy, aes(x = Datum, y = ret1)) +
#          geom_line() +  
#          labs(title = "log returnes h=1", x = "time", y = "log return")+
#          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#          theme_minimal() 
# )
# dev.off()

# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\DAX_ret2.pdf", width = 7, height = 4)
# print (ggplot(dummy, aes(x = Datum, y = ret2)) +
#          geom_line() +  
#          labs(title = "log returnes h=2", x = "time", y = "log return")+
#          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#          theme_minimal() 
# )
# dev.off()
# 
# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\DAX_ret3.pdf", width = 7, height = 4)
# print (ggplot(dummy, aes(x = Datum, y = ret3)) +
#          geom_line() +  
#          labs(title = "log returnes h=3", x = "time", y = "log return")+
#          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#          theme_minimal() 
# )
# dev.off()
# 
# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\DAX_ret4.pdf", width = 7, height = 4)
# print (ggplot(dummy, aes(x = Datum, y = ret4)) +
#          geom_line() +  
#          labs(title = "log returnes h=4", x = "time", y = "log return")+
#          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#          theme_minimal() 
# )
# dev.off()
# 
# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\DAX_ret5.pdf", width = 7, height = 4)
# print (ggplot(dummy, aes(x = Datum, y = ret5)) +
#          geom_line() +  
#          labs(title = "log returnes h=5", x = "time", y = "log return")+
#          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#          theme_minimal() 
# )
# dev.off()
