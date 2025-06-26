
#calibration = Feintuning
#Consistency, elicitibility (Erreichbarkeit) & propriety (Angemessenheit)



method_evaluation <- function (datum, data, forecasts){
  datum <- as.Date(datum)
  tau <-c(.025, .25, .5, .75, .975)
  gefilterte_gwh_werte <- data %>%
    dplyr::filter(Datum == datum, Anfang %in% c("12:00", "16:00", "20:00")) %>%
    select(GWh)
  actual <- gefilterte_gwh_werte
  print(actual)
  pred_1column <- data.frame(
    horizon = rep(rownames(forecasts), each = length(tau)),
    tau = factor(rep(tau, times = nrow(forecasts))),
    pred = as.vector(t(forecasts))
  )

  actualx5 <- c(rep(actual[1,], times = 5), rep(actual[2,], times = 5), rep(actual[3,], times = 5))
  evaluation_data <- cbind (pred_1column, actualx5)
  colnames(evaluation_data)[4] <- "actual"
  evaluation_data$quantile_score <- rep(1, times = 15)
  evaluation_data$pred <- as.numeric(as.character(evaluation_data$pred))
  evaluation_data$actual <- as.numeric(as.character(evaluation_data$actual))
  evaluation_data$tau <- as.numeric(as.character(evaluation_data$tau))

  # Quantile Score ----------------------------------------------------------
  quantile_score <- function(forecast, actual, tau) {
    
    ifelse(forecast > actual, 
           2 * (1 - tau) * (forecast - actual), 
           2 * tau * (actual - forecast))
  }
  #linear quantile scores for all different quantile levels & horizons:
  for (i in 1:length(evaluation_data$pred)){
    evaluation_data$quantile_score[i] = quantile_score (evaluation_data$pred[i], evaluation_data$actual[i], evaluation_data$tau[i])
  }
 
  # average linear quantile score:
  mean_qscore <- mean (evaluation_data$quantile_score)
  
print(mean_qscore)
# Mean Bias Error ---------------------------------------------------------
  mbe_data <- evaluation_data %>% filter (tau == 0.5)
  mbe <- mean(mbe_data$pred - mbe_data$actual)  
  
  # print (ggplot(pred_1column, aes(x = horizon, y = pred, group = horizon)) +
  #   geom_point(aes(color = tau)) +  # Punkte für jedes Quantil
  #   geom_line(aes(group = horizon)) +  # Linien verbinden Quantile desselben Horizonts
  #   scale_colour_manual(values=c("0.025"="darkblue", "0.25"="orange", "0.5"="green", "0.75"="orange", "0.975"="darkblue")) +
  #   geom_point(data = evaluation_data, aes(x = horizon, y = actual, group = horizon), color = "red", size = 3) +
  #   theme_minimal() +
  #   labs(title = "Quantil Vorhersagen + Realisierungen", x = "Horizont", y = "Energieverbrauch (GWh)")
  # )
return (c(mean_qscore = mean_qscore, mbe = mbe))

}

method_evaluation_2days <- function (datum, data, forecasts){
  datums <- c(datum, (datum + 1))
  tau <-c(.025, .25, .5, .75, .975)
  gefilterte_gwh_werte <- data %>% dplyr::filter(Datum %in% datums, Anfang %in% c("12:00", "16:00", "20:00")) %>% select(GWh)
  actual <- gefilterte_gwh_werte
  print(actual)
  pred_1column <- data.frame(
    horizon = rep(rownames(forecasts), each = length(tau)),
    tau = factor(rep(tau, times = nrow(forecasts))),
    pred = as.vector(t(forecasts))
  )

  actualx5 <- c(rep(actual$GWh[1], times = 5), rep(actual$GWh[2], times = 5), rep(actual$GWh[3], times = 5), rep(actual$GWh[4], times = 5), rep(actual$GWh[5], times = 5), rep(actual$GWh[6], times = 5))
  evaluation_data <- cbind (pred_1column, actualx5)
  colnames(evaluation_data)[4] <- "actual"
  evaluation_data$quantile_score <- rep(1, times = 30)
  evaluation_data$pred <- as.numeric(as.character(evaluation_data$pred))
  evaluation_data$actual <- as.numeric(as.character(evaluation_data$actual))
  evaluation_data$tau <- as.numeric(as.character(evaluation_data$tau))
  
  
  # Quantile Score ----------------------------------------------------------
  quantile_score <- function(forecast, actual, tau) {
    
    ifelse(forecast > actual, 
           2 * (1 - tau) * (forecast - actual), 
           2 * tau * (actual - forecast))
  }
  #linear quantile scores for all different quantile levels & horizons:
  for (i in 1:length(evaluation_data$pred)){
    evaluation_data$quantile_score[i] = quantile_score (evaluation_data$pred[i], evaluation_data$actual[i], evaluation_data$tau[i])
  }
  
  # average linear quantile score:
  mean_qscore <- mean (evaluation_data$quantile_score)
  
  # Mean Bias Error ---------------------------------------------------------
  mbe_data <- evaluation_data %>% filter (tau == 0.5)
  mbe <- mean(mbe_data$pred - mbe_data$actual)  
 
  return (c(mean_qscore = mean_qscore, mbe = mbe))
  
}
