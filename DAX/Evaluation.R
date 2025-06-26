
source("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\DAX\\dax_procs.R")
library(dplyr)


evaluation <- function (datum, data, forecasts){
  forecasts <- as.data.frame(forecasts)
  datum <- as.Date(datum)
  i <- which(data$Datum == datum)
  actual_ret1 <- as.numeric (data [(i+1) , "ret1"])
  actual_ret2 <- as.numeric (data [(i+2) , "ret2"])
  actual_ret3 <- as.numeric (data [(i+3) , "ret3"])
  actual_ret4 <- as.numeric (data [(i+4) , "ret4"])
  actual_ret5 <- as.numeric (data [(i+5) , "ret5"])
  actual <- c(actual_ret1, actual_ret2, actual_ret3, actual_ret4, actual_ret5)
  forecasts$actual <- actual
  tau <-c(.025, .25, .5, .75, .975)
  
  # Quantile Score ----------------------------------------------------------
  quantile_score <- function(forecast, actual, tau) {

      ifelse(forecast > actual, 
             2 * (1 - tau) * (forecast - actual), 
             2 * tau * (actual - forecast))
    }
  forecasts <- as.data.frame(forecasts)

  # linear quantile scores for all different quantile levels & horizons:
  for (i in 1:5){
    forecasts$quantile_score025[i] = quantile_score (forecasts[i,"q0.025"], forecasts$actual[i], 0.025)
    forecasts$quantile_score25[i] = quantile_score (forecasts[i,"q0.25"], forecasts$actual[i], 0.25)
    forecasts$quantile_score5[i] = quantile_score (forecasts[i,"q0.5"], forecasts$actual[i], 0.5)
    forecasts$quantile_score75[i] = quantile_score (forecasts[i,"q0.75"], forecasts$actual[i], 0.75)
    forecasts$quantile_score975[i] = quantile_score (forecasts[i,"q0.975"], forecasts$actual[i], 0.975)
  } 
  #average linear quantile score:
  mean_q025score <- mean (forecasts$quantile_score025, na.rm = TRUE)
  mean_q25score <- mean (forecasts$quantile_score25, na.rm = TRUE)
  mean_q5score <- mean (forecasts$quantile_score5, na.rm = TRUE)
  mean_q75score <- mean (forecasts$quantile_score75, na.rm = TRUE)
  mean_q975score <- mean (forecasts$quantile_score975, na.rm = TRUE)
  mean_qscore <- mean(mean_q025score, mean_q25score,mean_q5score, mean_q75score, mean_q975score, na.rm = TRUE)

  return (mean_qscore )
  
}







