library(dplyr)
library(tidyr)
library(tsibble)
library(fable)
library(fpp3)
setwd ("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy")
source("Energy_Code\\Energy_methods.R") 
source("Energy_Code\\Evaluation.R")

tau <-c(.025, .25, .5, .75, .975)
load("all_data.Rdata")

tsibble_data <- as_tsibble(all_data, index = DateTime)

tsibble_data <- tsibble_data %>% fill_gaps() %>% fill(everything(), .direction = "down")


#Unterteilung in Train, Validation und Testset
n <- nrow(tsibble_data)
n_train <- floor(0.7 * n)
n_val <- floor(0.1 * n) + n_train
dynamic_train_data <- tsibble_data[1:n_train, ]
dynamic_val_data <- tsibble_data[(n_train + 1):n_val, ]
dynamic_test_data <- tsibble_data[(n_val + 1):n, ]



# Modelle erstellen --------------------------------------------------------
#dynamic_model_small <- dynamic_train_data %>% model (ARIMA (GWh ~ Anfang + month + day + holiday))
#load("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy\\dynamic_model.Rdata")
#print(dynamic_model)

# dynamic_model_hol_sun_temp <- dynamic_train_data %>% model(ARIMA(GWh ~ holiday + sun + temp))
#load("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy\\dynamic_model_hol_sun_temp.Rdata")

#dynamic_model_time_hol_sun_temp <- dynamic_train_data %>% model(ARIMA(GWh ~ Anfang + holiday + sun + temp))
#load("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy\\dynamic_model_time_hol_sun_temp.Rdata")

#dynamic_model_time_hol_sun_temp_squared <- dynamic_train_data %>% model(ARIMA(GWh ~ Anfang + holiday + sun + temp + I(temp^2)))
#load("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy\\dynamic_model_time_hol_sun_temp_squared.Rdata")

#dynamic_model_time_month_hol_sun_temp <- dynamic_train_data %>% model(ARIMA(GWh ~ Anfang + month + holiday + sun + temp))
#load("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy\\dynamic_model_time_month_hol_sun_temp.Rdata")

# forecast <- forecast(dynamic_model, new_data = dynamic_val_data)
# res <- residuals(dynamic_model)
# jarque.bera.test(na.omit(res$.resid))
# hist(na.omit(res$.resid))
# print(forecast)
# report(dynamic_model)



# Mit Rolling Window Forecasts erstellen ----------------------------------

dynamic_forecasts <- function( dataset){

  index <- which(dynamic_test_data$DateTime == dataset$DateTime[nrow(dataset)])
  new_data_forecast <- dynamic_test_data[(index+1):(index+100), ]
  print(dataset)
  
  model <- dataset %>% model(ARIMA(GWh ~ Anfang + month + holiday + day + pdq (4, 1, 0) + PDQ (2, 0, 0)))
  print(model)
  if (any(is.na(residuals(model)$.resid))) {
    return(NA)
  } else {
 
  forecast <- forecast(model, new_data = new_data_forecast)
  print(forecast)
  parameters_list <- distributional::parameters(forecast$GWh)
  
  # h=36
  mean_value_36 <- parameters_list[36, 1] # mu bekommen
  sd_value_36 <- parameters_list[36, 2] # sigma bekommen
  forecasts_36 <- qnorm(tau, mean_value_36, sd_value_36)
  
  # für h=40
  mean_value_40 <- parameters_list[40, 1]
  sd_value_40 <- parameters_list[40, 2]
  forecasts_40 <- qnorm(tau, mean_value_40, sd_value_40)
  
  # für h=44
  mean_value_44 <- parameters_list[44, 1] 
  sd_value_44 <- parameters_list[44, 2] 
  forecasts_44 <- qnorm(tau, mean_value_44, sd_value_44)
  
  # für h=60
  mean_value_60 <- parameters_list[60, 1] 
  sd_value_60 <- parameters_list[60, 2] 
  forecasts_60 <- qnorm(tau, mean_value_60, sd_value_60)
  
  # für h=64
  mean_value_64 <- parameters_list[64, 1] 
  sd_value_64 <- parameters_list[64, 2] 
  forecasts_64 <- qnorm(tau, mean_value_64, sd_value_64)
  
  # für h=68
  mean_value_68 <- parameters_list[68, 1] 
  sd_value_68 <- parameters_list[68, 2] 
  forecasts_68 <- qnorm(tau, mean_value_68, sd_value_68)
  
  predictions <- as.data.frame(rbind(forecasts_36, forecasts_40, forecasts_44, 
                                     forecasts_60, forecasts_64, forecasts_68))
  colnames(predictions) = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  
  return(predictions)
}}

dynamic_evaluation_matrix <- dynamic_test_data
dynamic_evaluation_matrix$QS <- NA
dynamic_evaluation_matrix$BE <- NA

for(i in 4900:5000 ) {
  print (paste0("Fortschritt: ", i/ nrow(dynamic_test_data))) # Fortschrittsanzeige
  dataset <- bind_rows(dynamic_val_data, dynamic_test_data[1:i, ])
  dataset <- tail(dataset, 9000)
  if (dynamic_test_data$Anfang[i] == "00:00"){
    forecasts_dynamic <- dynamic_forecasts(dataset)
    print(forecasts_dynamic)
    if (!any(is.na(forecasts_dynamic))) {
    evaluation <- method_evaluation_2days(datum= as.Date(dynamic_test_data$Datum[i]), data=dynamic_test_data, forecasts=forecasts_dynamic)
    print(evaluation)
    dynamic_evaluation_matrix$QS[i] <- evaluation[1]
    dynamic_evaluation_matrix$BE[i] <- evaluation[2]
  }}
}                 

(mean(dynamic_evaluation_matrix$QS, na.rm = TRUE))
(mean(dynamic_evaluation_matrix$BE, na.rm = TRUE))


# Archiv ------------------------------------------------------------------


#library(fpp3)
#pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\gg_season_day.pdf", width = 7, height = 4)
#tsibble_data %>% gg_season(GWh, period = "")
#dev.off()

# residuals <- residuals(dynamic_model)
# qqnorm(residuals$.resid)
# qqline(residuals$.resid)

# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\gg_subseason.pdf", width = 7, height = 4)
# tsibble_data %>% gg_subseries(GWh)
# dev.off()

#Histogramm mit Normalverteilungskurve
# ggplot(df, aes(x=resid_values)) +
#   geom_histogram(aes(y=..density..), binwidth=0.5, fill="darkgrey", alpha=0.5) +
#   stat_function(fun=dnorm, args=list(mean=mean(df$resid_values), sd=sd(df$resid_values)), color="black", size=0.6) +
#   ggtitle("Histogram of residuals with normal distribution curve") +
#   xlab("Residuals") + ylab("Density")
# dev.off()