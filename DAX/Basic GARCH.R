
library(dplyr)
library(lubridate)
library(rugarch)
library(zoo)
library(tseries)

#setwd("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\DAX")
#source("dax_procs.R")
#source("Evaluation.R")
tau <- c(.025, .25, .5, .75, .975) # quantile levels

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
# Box.test((dat_returns$ret1)^2, type =c ("Ljung-Box"))

n <- nrow(dat_returns)
n_train <- floor(0.7 * n)
n_val <- floor(0.1 * n) + n_train
garch_train_data <- dat_returns[1:n_train, ]
garch_val_data <- dat_returns[(n_train + 1):n_val, ]
garch_test_data <- dat_returns[(n_val + 1):n, ]


# Modelle aufstellen ------------------------------------------------------

garch_spec_h1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(1,0)), 
                            distribution.model = "norm" )
garch_fit_h1 <- ugarchfit (data = ts(garch_train_data$ret1), spec = garch_spec_h1)
print(garch_fit_h1)

garch_spec_h2 <- ugarchspec(variance.model = list(model = "sGARCH",  garchOrder = c(2, 1)),
                            mean.model = list(armaOrder = c(4,2)), 
                            distribution.model = "norm" )
garch_fit_h2 <- ugarchfit (data = ts(garch_train_data$ret2), spec = garch_spec_h2)


garch_spec_h3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
                            mean.model = list(armaOrder = c(4,1)), 
                            distribution.model = "norm" )
garch_fit_h3 <- ugarchfit (data = ts(garch_train_data$ret3), spec = garch_spec_h3)

garch_spec_h4 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(5,3)), 
                            distribution.model = "norm" )
garch_fit_h4 <- ugarchfit (data = ts(garch_train_data$ret4), spec = garch_spec_h4)

garch_spec_h5 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(6,3)), 
                            distribution.model = "norm" )
garch_fit_h5 <- ugarchfit (data = ts(garch_train_data$ret5), spec = garch_spec_h5)


#Boot strap variante wurde nicht weiter verwendet
garch_forecasts_bootstrap <- function(dataset){
  
  # für h=1
  garch_fit_h1 <- ugarchfit (data = ts(dataset$ret1), spec = garch_spec_h1)
  if( !(garch_fit_h1@fit$convergence == 0)){
    return(NA)
  }
  forecasts_1 <-numeric(5)
  for(i in tau){
  bootpred = ugarchboot(garch_fit_h1, method = "Partial", n.ahead = 1, n.bootpred = 1000)
  fseries <- apply(bootpred@fseries, 2, quantile, probs = i)
  index <- match(i, tau)
  forecasts_1[index] <- fseries[1]
  }
  # für h=2
  garch_fit_h2 <- ugarchfit (data = ts(dataset$ret2), spec = garch_spec_h2)
  if( !(garch_fit_h2@fit$convergence == 0)){
    return(NA)
  }
  forecasts_2 <-numeric(5)
  for(i in tau){
    bootpred = ugarchboot(garch_fit_h2, method = "Partial", n.ahead = 2, n.bootpred = 1000)
    fseries <- apply(bootpred@fseries, 2, quantile, probs = i)
    index <- match(i, tau)
    forecasts_2[index] <- fseries[2]
  }
  # für h=3
  garch_fit_h3 <- ugarchfit (data = ts(dataset$ret3), spec = garch_spec_h3)
  if( !(garch_fit_h3@fit$convergence == 0)){
    return(NA)
  }
  forecasts_3 <-numeric(5)
  for(i in tau){
    bootpred = ugarchboot(garch_fit_h3, method = "Partial", n.ahead = 3, n.bootpred = 1000)
    fseries <- apply(bootpred@fseries, 2, quantile, probs = i)
    index <- match(i, tau)
    forecasts_3[index] <- fseries[3]
  }
  # für h=4
  garch_fit_h4 <- ugarchfit (data = ts(dataset$ret4), spec = garch_spec_h4)
  if( !(garch_fit_h4@fit$convergence == 0)){
    return(NA)
  }
  forecasts_4 <-numeric(5)
  for(i in tau){
    bootpred = ugarchboot(garch_fit_h4, method = "Partial", n.ahead = 4, n.bootpred = 1000)
    fseries <- apply(bootpred@fseries, 2, quantile, probs = i)
    index <- match(i, tau)
    forecasts_4[index] <- fseries[4]
  }
  # für h=5
  garch_fit_h5 <- ugarchfit (data = ts(dataset$ret5), spec = garch_spec_h5)
  if( !(garch_fit_h5@fit$convergence == 0)){
    return(NA)
  }
  forecasts_5 <-numeric(5)
  for(i in tau){
    bootpred = ugarchboot(garch_fit_h5, method = "Partial", n.ahead = 5, n.bootpred = 1000)
    fseries <- apply(bootpred@fseries, 2, quantile, probs = i)
    index <- match(i, tau)
    forecasts_5[index] <- fseries[5]
  }
  
  predictions <- as.data.frame(rbind(forecasts_1, forecasts_2, forecasts_3, forecasts_4, forecasts_5))
  colnames(predictions) = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  rownames(predictions) = c("ret1", "ret2", "ret3", "ret4", "ret5")
  
  return(predictions)
}
garch_forecasts <- function(dataset){
  
  # für h=1
  garch_fit_h1 <- ugarchfit (data = ts(dataset$ret1), spec = garch_spec_h1)
  if( !(garch_fit_h1@fit$convergence == 0)){
    return(NA)
  }
  forc_1 <- ugarchforecast(fitORspec = garch_fit_h1, n.ahead = 1)
  forecasts_1 <- qnorm(tau, mean = forc_1@forecast$seriesFor[1], sd = forc_1@forecast$sigmaFor[1])
  
  # für h=2
  garch_fit_h2 <- ugarchfit (data = ts(dataset$ret2), spec = garch_spec_h2)
  if( !(garch_fit_h2@fit$convergence == 0)){
    return(NA)
  }
  forc_2 <- ugarchforecast(fitORspec = garch_fit_h2, n.ahead = 2)
  forecasts_2 <- qnorm(tau, mean = forc_2@forecast$seriesFor[2], sd = forc_2@forecast$sigmaFor[2])
  # für h=3
  garch_fit_h3 <- ugarchfit (data = ts(dataset$ret3), spec = garch_spec_h3)
  if( !(garch_fit_h3@fit$convergence == 0)){
    return(NA)
  }
  forc_3 <- ugarchforecast(fitORspec = garch_fit_h3, n.ahead = 3)
  forecasts_3 <- qnorm(tau, mean = forc_3@forecast$seriesFor[3], sd = forc_3@forecast$sigmaFor[3])
  # für h=4
  garch_fit_h4 <- ugarchfit (data = ts(dataset$ret4), spec = garch_spec_h4)
  if( !(garch_fit_h4@fit$convergence == 0)){
    return(NA)
  }
  forc_4 <- ugarchforecast(fitORspec = garch_fit_h4, n.ahead = 4)
  forecasts_4 <- qnorm(tau, mean = forc_4@forecast$seriesFor[4], sd = forc_4@forecast$sigmaFor[4])
  # für h=5
  garch_fit_h5 <- ugarchfit (data = ts(dataset$ret5), spec = garch_spec_h5)
  if( !(garch_fit_h5@fit$convergence == 0)){
    return(NA)
  }
  forc_5 <- ugarchforecast(fitORspec = garch_fit_h5, n.ahead = 5)
  forecasts_5 <- qnorm(tau, mean = forc_5@forecast$seriesFor[5], sd = forc_5@forecast$sigmaFor[5])

  predictions <- as.data.frame(rbind(forecasts_1, forecasts_2, forecasts_3, forecasts_4, forecasts_5))
  colnames(predictions) = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  rownames(predictions) = c("ret1", "ret2", "ret3", "ret4", "ret5")
  
  
  garch_fit_h5 <- ugarchfit (data = ts(garch_train_data$ret5), spec = garch_spec_h5)
  forc_5 <- ugarchforecast(fitORspec = garch_fit_h5, n.ahead = 5)
  forecasts_5 <- qnorm(tau, mean = forc_5@forecast$seriesFor[5], sd = forc_5@forecast$sigmaFor[5])
  print(forecasts_5)
  return(predictions)
}

garch_evaluation_matrix <- garch_val_data
garch_evaluation_matrix$QS <- NA
forecasts_garch <- garch_forecasts(bind_rows(garch_train_data, garch_val_data[1:53,]))

for(i in 1: (nrow(garch_val_data) - 6)) {
  print (paste0("Fortschritt: ", i/ nrow(garch_val_data))) #Fortschrittsanzeige
  dataset <- bind_rows(garch_train_data, garch_val_data[1:i,])
  dataset <- tail(dataset, 365)
  forecasts_garch <- garch_forecasts(dataset)
  print(forecasts_garch)
  if(!any(is.na(forecasts_garch))){
  qs_score <- evaluation(datum= dataset$Datum[nrow(dataset)], data=garch_val_data, forecasts=forecasts_garch)
  print(qs_score)
  garch_evaluation_matrix$QS[i] <- qs_score
  }
}     

print(mean(garch_evaluation_matrix$QS, na.rm = TRUE))


# forc <- garch_forecasts(data = dat_returns)
# print(forc)

