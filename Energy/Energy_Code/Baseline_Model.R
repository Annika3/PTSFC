#Quasi Baseline Model aus Ilias, umgeschrieben in R und auf Basis von heruntergeladenen Werten und nicht direkt von der Website
#Kurzusammenfassung Baselinemodel: Vorhersage basiert auf historischen Daten am gleichen Wochentag + Uhrzeit 
#+ nur die letzten 100 Daten hiervon werden betrachtet


library(dplyr)
#Set Working Directory
setwd ("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy")
#Verknüpfung zu Methoden File
source("Energy_Code\\Energy_methods.R") 
source("Energy_Code\\Evaluation.R") 

tau <-c(.025, .25, .5, .75, .975)

load("all_data.Rdata")
data_baseline <- all_data

method_baseline_forecasts <- function(day, data){
  forecast_weekday <- format(day, "%A")
 
  data_day <- data %>% filter (day == forecast_weekday)
  
  pred_baseline <- matrix(0, nrow = 3, ncol = 5)
  rownames(pred_baseline) <- c("12:00", "16:00", "20:00")
  colnames(pred_baseline) <- c("2.5%", "25%", "50%", "75%", "97.5%")
  
  
  data_12 <- data_day %>% filter (Anfang == "12:00")
  data_16 <- data_day %>% filter (Anfang == "16:00")
  data_20 <- data_day %>% filter (Anfang == "20:00")
  
  
  data_12_baseline <- tail(data_12, 100)
  data_16_baseline <- tail(data_16, 100)
  data_20_baseline <- tail(data_20, 100)
  
  pred_baseline[1, ] <- quantile(data_12_baseline$GWh, probs = tau, na.rm = TRUE)
  pred_baseline[2, ] <- quantile(data_16_baseline$GWh, probs = tau, na.rm = TRUE)
  pred_baseline[3, ] <- quantile(data_20_baseline$GWh, probs = tau, na.rm = TRUE)
  
  return (pred_baseline)
}

test_oneday <- test_data %>% filter(Anfang %in% c("12:00"))
evaluation_matrix_baseline.df <- data.frame(matrix(ncol = 9, nrow = 0))

for (i in 2:nrow(test_oneday)){
  data_filtered <- test_data %>% filter(Datum < as.Date(test_oneday$Datum[i]))
  print(data_filtered)
  pred_baseline <- method_baseline_forecasts(as.Date(test_oneday$Datum[i]), data_filtered)
  print(pred_baseline)
  evaluation_baseline <- method_evaluation(as.Date(test_oneday$Datum[i]), test_data, pred_baseline)
  test_oneday$QS[i] <- evaluation_baseline[1]
  print(evaluation_baseline[1])
  test_oneday$BE[i] <- evaluation_baseline[2]
  print(evaluation_baseline[2])
  #dummy_matrix.df <- data.frame(cbind (rep(as.Date(test_oneday$Datum[i]), 3), pred_baseline, c(evaluation_baseline[3], evaluation_baseline[4], evaluation_baseline[5]), rep(evaluation_baseline[1], 3), rep(evaluation_baseline[2], 3)))
 # evaluation_matrix_baseline.df <- rbind(evaluation_matrix_baseline.df, dummy_matrix.df)
}

colnames(evaluation_matrix_baseline.df) <- c("Datum", "2.5%", "25%", "50%", "75%", "97.5%", "actual", "Quantile Score", "Bias Error")
evaluation_matrix_baseline.df$Datum <- as.Date(evaluation_matrix_baseline.df$Datum) #Datumsspalte in richtiges Format bringen

tail(evaluation_matrix_baseline.df)

(mean_qs_baseline <- mean(test_oneday$QS, na.rm = TRUE))
(mean_be_baseline <- mean(test_oneday$BE, na.rm = TRUE))






# Plotten -----------------------------------------------------------------

library(ggplot2)
plot_df <- data.frame(
  horizon = rep(rownames(pred_baseline), each = length(tau)),
  tau = factor(rep(tau, times = nrow(pred_baseline))),
  pred = as.vector(t(pred_baseline))
)
plot_df

ggplot(plot_df, aes(x = horizon, y = pred, group = horizon)) +
  geom_point(aes(color = tau)) +  
  geom_line(aes(group = horizon, color = tau)) +  
  theme_minimal() +
  labs(title = "Quantilbasierte Vorhersagen", x = "Horizont", y = "Energieverbrauch (GWh)")
