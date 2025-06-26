library(dplyr)
library(tidyr)
library(quantreg)
setwd ("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy")
source("Energy_Code\\Energy_methods.R") 
source("Energy_Code\\Evaluation.R")

load("all_data.Rdata")

tau <-c(.025, .25, .5, .75, .975)


#Unterteilung in Test & Train
set.seed(123)
unique_dates <- unique(all_data$Datum)
num_train_val_dates <- round(length(unique_dates) * 0.7)
train_val_dates <- sample(unique_dates, num_train_val_dates)
train_val_data <- all_data[all_data$Datum %in% train_val_dates, ]
test_data <- all_data[!all_data$Datum %in% train_val_dates, ]

#Unterteilung train und validate
unique_train_dates <- unique(train_val_data$Datum)
num_train_dates <- round(length(unique_train_dates) * 0.8)
train_dates <- sample(unique_train_dates, num_train_dates)
train_data <- train_val_data[train_val_data$Datum %in% train_dates, ]
val_data <- train_val_data[!train_val_data$Datum %in% train_dates, ]


# Modelle schätzen ---------------------------------------------------------
method_seasonal_quantile <- function (train_data, tau_level){
  model <- rq(GWh ~ Anfang + month + holiday + day, data = train_data, tau = tau_level)
  return (model)
}

seasonal_model_list <- list(
  "0.025" = method_seasonal_quantile(train_data, 0.025),
  "0.25" = method_seasonal_quantile(train_data, 0.25),
  "0.5" = method_seasonal_quantile(train_data, 0.5),
  "0.75" = method_seasonal_quantile(train_data, 0.75),
  "0.975" = method_seasonal_quantile(train_data, 0.975)
)


seasonal_quantreg_forecasts <- function (d, data){
  month <- format(d, "%B")
  year <- format(d, "%Y")
  day <- format(d, "%A")
  variableday <- paste0("day", day)
  variablemonth <- paste0("month", month)
  
  pred_seasonal <- matrix(0, nrow = 3, ncol = 5)
  rownames(pred_seasonal) <- c("12:00", "16:00", "20:00")
  colnames(pred_seasonal) <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  
  for (i in tau){
    string_tau <- as.character(i)
    model <- seasonal_model_list[[string_tau]]
    data_12 <- data %>% dplyr::filter(Datum == as.Date(d) & format(Anfang, format = "%H:%M") == "12:00")
    data_16 <- data %>% dplyr::filter(Datum == as.Date(d) & format(Anfang, format = "%H:%M") == "16:00")
    data_20 <- data %>% dplyr::filter(Datum == as.Date(d) & format(Anfang, format = "%H:%M") == "20:00")
    
    point_12 <- predict(model, data_12)
    point_16 <- predict(model, data_16)
    point_20 <- predict(model, data_20)
    
    pred_seasonal["12:00", string_tau] <- point_12
    pred_seasonal["16:00", string_tau] <- point_16
    pred_seasonal["20:00", string_tau] <- point_20
  }
  
  return (pred_seasonal)
}


# Validation Testing ------------------------------------------------------

val_oneday <- val_data %>% filter(Anfang == "00:00") #Subset mit jeweils nur einer Zeile pro Tag (damit jedes val - Datum nur einmal vorkommt)
val_oneday <- val_oneday %>% filter(holiday == "FALSE") #Feiertage entfernen
validation_matrix_quantreg.df <- data.frame(matrix(ncol = 9, nrow = 0))
for (i in 1:nrow(val_oneday)){
  print(i/nrow(val_oneday))
  pred_quantreg <- seasonal_quantreg_forecasts(as.Date(val_oneday$Datum[i]), val_data)
  evaluation_quantreg <- method_evaluation(as.Date(val_oneday$Datum[i]), val_data, pred_quantreg)
  val_oneday$QS[i] <- evaluation_quantreg[1]
  print(evaluation_quantreg[1])
  val_oneday$BE[i] <- evaluation_quantreg[2]
  print(evaluation_quantreg[2])
}

colnames(validation_matrix_quantreg.df) <- c("Datum", "2.5%", "25%", "50%", "75%", "97.5%", "actual", "Quantile Score", "Bias Error")
validation_matrix_quantreg.df$Datum <- as.Date(validation_matrix_quantreg.df$Datum) #Datumsspalte in richtiges Format bringen

(mean_qs_seasonal_quantreg <- mean(val_oneday$QS, na.rm = TRUE))
(mean_be_seasonal_quantreg <- mean(val_oneday$BE, na.rm = TRUE))


# Testen anhand Testdatensatz ---------------------------------------------
'
test_oneday <- test_data %>% filter(Anfang == "00:00") #Subset mit jeweils nur einer Zeile pro Tag (damit jedes Test - Datum nur einmal vorkommt)
test_oneday <- test_oneday %>% filter(holiday == "FALSE") #Feiertage entfernen
evaluation_matrix_quantreg.df <- data.frame(matrix(ncol = 9, nrow = 0))
for (i in 1:nrow(test_oneday)){
  print(i)
  pred_quantreg <- seasonal_quantreg_forecasts(as.Date(test_oneday$Datum[i], test_data))
  evaluation_quantreg <- method_evaluation(as.Date(test_oneday$Datum[i]), test_data, pred_quantreg)
  dummy_matrix.df <- data.frame(cbind (rep(as.Date(test_oneday$Datum[i]), 3), pred_quantreg, c(evaluation_quantreg[3], evaluation_quantreg[4], evaluation_quantreg[5]), rep(evaluation_quantreg[1], 3), rep(evaluation_quantreg[2], 3)))
  evaluation_matrix_quantreg.df <- rbind(evaluation_matrix_quantreg.df, dummy_matrix.df)
}

colnames(evaluation_matrix_quantreg.df) <- c("Datum", "2.5%", "25%", "50%", "75%", "97.5%", "actual", "Quantile Score", "Bias Error")
evaluation_matrix_quantreg.df$Datum <- as.Date(evaluation_matrix_quantreg.df$Datum) #Datumsspalte in richtiges Format bringen

(mean_qs_seasonal_quantreg <- mean(evaluation_matrix_quantreg.df$`Quantile Score`, na.rm = TRUE))
(mean_be_seasonal_quantreg <- mean(evaluation_matrix_quantreg.df$`Bias Error`, na.rm = TRUE))
'








