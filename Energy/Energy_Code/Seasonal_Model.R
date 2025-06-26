library(dplyr)
library(tidyr)
library(ggplot2)
setwd ("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy")
source("Energy_Code\\Energy_methods.R") 
source("Energy_Code\\Evaluation.R")

tau <-c(.025, .25, .5, .75, .975)

## Um Rechenzeit zu sparen wurde der Datensatz , der aus den folgenden Anweisungen resultiert, als RData zwischengespeichert und wird nur noch geladen
'
csv1 <- read.csv("Energy_Daten\\Realisierter_Stromverbrauch_2020-2024_Stunde.csv", sep = ";")
csv2 <- read.csv("Energy_Daten\\Realisierter_Stromverbrauch_2015-2019_Stunde.csv", sep = ";")
csv_data <- rbind(csv2, csv1)
#Daten modifizieren
all_data_dummys <- modify_data(csv_data)
all_data_dummys$DateTime <- with(all_data_dummys, paste(Datum, Anfang)) %>%
  lubridate::ymd_hm()

temp <- get_temp()
sun <- get_sun()

#Duplikate aus Zeitumstellungen entfernen
all_data_dummys <- all_data_dummys[!duplicated(all_data_dummys$DateTime), ]
temp <- temp[!duplicated(temp$MESS_DATUM), ]
sun <- sun[!duplicated(sun$MESS_DATUM), ]
#3 Datensätze kombinieren
merged <- left_join(all_data_dummys, temp, by = c("DateTime" = "MESS_DATUM"))
all_data <- left_join(merged, sun, by = c("DateTime" = "MESS_DATUM"))

all_data <- all_data %>% rename("temp" = "TT_TU.Lufttemperatur", "sun" = "SD_SO.Sonnenscheindauer")
all_data$sun[is.na(all_data$sun)] <- 0 #Da NA Werte nur den in Nachtstunden waren, können sie mit 0 ersetzt werden
save(all_data, file = "all_data.Rdata")
'

load("all_data.Rdata")


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


# Modell schätzen ---------------------------------------------------------
method_seasonal_model <- function (training_data){
  model <- lm(GWh ~ Anfang + month + holiday + day + sun + temp + I(temp^2), data = training_data)
  return (model)
}
jarque.bera.test(all_data$GWh)

seasonal_model <- method_seasonal_model(train_data)



# Forecasts erstellen & gleichzeitig evaluieren -----------------------------------------------------

# new_daten <- data.frame( Datum = as.Date("xxx"), Anfang = c("12:00", "16:00", "20:00"), day = "Freitag", month = "Januar", holiday = FALSE)
# print(new_daten)
# forc <- seasonal_forecasts(as.Date("xx"), seasonal_model, data = new_daten)
# print(forc)

seasonal_forecasts <- function (d, model, data, B=1000){
  
  data_12_d1 <- data %>% dplyr::filter(Datum == d & format(Anfang, format = "%H:%M") == "12:00")
  data_16_d1 <- data %>% dplyr::filter(Datum == d & format(Anfang, format = "%H:%M") == "16:00")
  data_20_d1 <- data %>% dplyr::filter(Datum == d & format(Anfang, format = "%H:%M") == "20:00")

  
  point_12 <- predict(model, data_12_d1)
  point_16 <- predict(model, data_16_d1)
  point_20 <- predict(model, data_20_d1)
  
  #Bootstrapping
  residuals <- residuals(model)
  bootstrap_pred <- array(dim = c(B, 3))

  for(i in 1:B) {
   
    sample_residuals <- sample(residuals, length(residuals), replace = TRUE)
  
    bootstrap_pred[i, 1] <- point_12 + sample(sample_residuals, 1)  
    bootstrap_pred[i, 2] <- point_16 + sample(sample_residuals, 1)  
    bootstrap_pred[i, 3] <- point_20 + sample(sample_residuals, 1)
    
  }
  
  pred_seasonal_bootstrap <- apply(bootstrap_pred, 2, function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
  
  pred_seasonal <- matrix(unlist(pred_seasonal_bootstrap), nrow = 3, byrow = TRUE)
  rownames(pred_seasonal) <- c("12:00", " 16:00", "20:00")
  colnames(pred_seasonal) <- c("2.5%", "25%", "50%", "75%", "97.5%")
  return (pred_seasonal)
}

# Feintuning mithilfe Validierungsdatensatz -------------------------------
validation_oneday <- val_data %>% filter(Anfang == "00:00") #Subset mit jeweils nur einer Zeile pro Tag (damit jedes Test - Datum nur einmal vorkommt)
validation_oneday <- validation_oneday %>% filter(holiday == "FALSE")
validation_oneday$QS <- NA
validation_oneday$BE <- NA

for (i in 1:nrow(validation_oneday)){
  print(i/ (nrow(validation_oneday))) #Fortschrittsanzeige
  pred_seasonal <- normal_method_seasonal_forecasts(as.Date(validation_oneday$Datum[i]), seasonal_model)#, val_data)
  evaluation_seasonal <- method_evaluation(as.Date(validation_oneday$Datum[i]), val_data, pred_seasonal)
  validation_oneday$QS[i] <- evaluation_seasonal[1]
  print(evaluation_seasonal[1])
  validation_oneday$BE[i] <- evaluation_seasonal[2]
  print(evaluation_seasonal[2])
  #dummy_matrix.df <- data.frame(cbind (rep(as.Date(validation_oneday$Datum[i]), 3), pred_seasonal, c(evaluation_seasonal[3], evaluation_seasonal[4], evaluation_seasonal[5]), rep(evaluation_seasonal[1], 3), rep(evaluation_seasonal[2], 3)))
  #validation_matrix.df <- rbind(validation_matrix.df, dummy_matrix.df)
}

mean_qs_seasonal <- mean(validation_oneday$QS, na.rm = TRUE)
cat("Der mittlere Quantile Score für das Lineare Regressionsmodell beträgt: ", mean_qs_seasonal)
mean_be_seasonal <- mean(validation_oneday$BE, na.rm = TRUE)
cat("Der mittlere Bias Error für das Lineare Regressionsmodell beträgt: ", mean_be_seasonal)

# Scores mithilfe des Testdatensatzes bestimmen --------------------------

test_oneday <- test_data %>% filter(Anfang == "00:00") #Subset mit jeweils nur einer Zeile pro Tag (damit jedes Test - Datum nur einmal vorkommt)
test_oneday <- test_oneday %>% filter(holiday == "FALSE") #Feiertage entfernen
evaluation_matrix.df <- data.frame(matrix(ncol = 9, nrow = 0))

for (i in 1:nrow(test_oneday)){
  pred_seasonal <- seasonal_forecasts(as.Date(test_oneday$Datum[i]), seasonal_model, test_data)
  evaluation_seasonal <- method_evaluation(as.Date(test_oneday$Datum[i]), test_data, pred_seasonal)
  dummy_matrix.df <- data.frame(cbind (rep(as.Date(test_oneday$Datum[i]), 3), pred_seasonal, c(evaluation_seasonal[3], evaluation_seasonal[4], evaluation_seasonal[5]), rep(evaluation_seasonal[1], 3), rep(evaluation_seasonal[2], 3)))
  evaluation_matrix.df <- rbind(evaluation_matrix.df, dummy_matrix.df)
}

colnames(evaluation_matrix.df) <- c("Datum", "2.5%", "25%", "50%", "75%", "97.5%", "actual", "Quantile Score", "Bias Error")
evaluation_matrix.df$Datum <- as.Date(evaluation_matrix.df$Datum) #Datumsspalte in richtiges Format bringen

mean_qs_seasonal <- mean(evaluation_matrix.df$`Quantile Score`, na.rm = TRUE)
cat("Der mittlere Quantile Score für das Lineare Regressionsmodell beträgt: ", mean_qs_seasonal)
mean_be_seasonal <- mean(evaluation_matrix.df$`Bias Error`, na.rm = TRUE)
cat("Der mittlere Bias Error für das Lineare Regressionsmodell beträgt: ", mean_be_seasonal)


# Baukasten zur Untersuchung der Schätzungen und Scoring Werte ---------------------------------------------------
'
evaluation_matrix.df$day <- strftime(evaluation_matrix.df$Datum, format = "%A")#Spalte mit Wochentag hinzufügen
evaluation_matrix.df$day <- factor(evaluation_matrix.df$day, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
evaluation_matrix.df$month <- strftime(evaluation_matrix.df$Datum, format = "%B") #Spalte mit Monat hinzufügen
evaluation_matrix.df$month <- factor(evaluation_matrix.df$month, levels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))
evaluation_matrix.df$year <- strftime(evaluation_matrix.df$Datum, format = "%Y") #Spalte mit Jahr hinzufügen

outlier_data <- evaluation_matrix.df %>% filter(`Quantile Score` > 5)#Problem: Ausreißer sind auch schon in Trainingsdatenset
print (ggplot(evaluation_matrix.df, aes(x = month, y = `Quantile Score`)) +
         geom_boxplot() +  
         labs(title = "Quantile Scores", x = "month", y = "quantile score")+
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)
pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\QS_Year_Seasonal.pdf", width = 7, height = 4)
print (ggplot(evaluation_matrix.df, aes(x = year, y = `Quantile Score`)) +
         geom_boxplot() +  
         labs(title = "Quantile Scores per Test Year", x = "year", y = "quantile score")+
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)
dev.off()
pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\All Data_Seasonal.pdf", width = 7, height = 4)
print (ggplot(all_data, aes(x = day, y = GWh)) +
         geom_boxplot() +  
         labs(title = "GWh per day", x = "day", y = "GWh")+
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)
dev.off()
pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\Bias_Error_Seasonal_V1.pdf", width = 7, height = 4)
print (ggplot(evaluation_matrix.df, aes(x = month, y = `Bias Error`)) +
         geom_hline(yintercept = 0, linetype = "solid", color = "black") +
         geom_boxplot() +  
         labs(title = "Bias Error", x = "month", y = "quantile score")+
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)
dev.off()

pred_seasonal_1column <- data.frame(
  horizon = rep(rownames(pred_seasonal), each = length(tau)),
  tau = factor(rep(tau, times = nrow(pred_seasonal))),
  pred = as.vector(t(pred_seasonal))
)



# # Archiv: Forecasts mit manuellem Rausziehen der Koeffizienten ----------

'seasonal_forecasts <- function (d, model, B=1000){
  month <- format(d, "%B")
  year <- format(d, "%Y")
  day <- format(d, "%A")
  variableday <- paste0("day", day)
  variablemonth <- paste0("month", month)
  summary <- summary(model)
  
  #Koeffizientenwerte rausziehen
  Intercept <- summary$coefficients["(Intercept)", "Estimate"]
  if (variablemonth != "monthJanuar") {
    month_coeff <- summary$coefficients[variablemonth, "Estimate"]
  }
  if (variableday != "dayMontag") {
    day_coeff <- summary$coefficients[variableday, "Estimate"]
  }
  time_12 <- summary$coefficients["Anfang12:00", "Estimate"]
  time_16 <- summary$coefficients["Anfang16:00", "Estimate"]
  time_20 <- summary$coefficients["Anfang20:00", "Estimate"]
  
  #Point Forecasts
  if (variablemonth == "monthJanuar" && variableday =="dayMontag"){
    point_12 <- Intercept + time_12 
    point_16 <- Intercept + time_16 
    point_20 <- Intercept + time_20 
  }else if(variableday == "dayMontag"){
    point_12 <- Intercept + time_12 + month_coeff 
    point_16 <- Intercept + time_16 + month_coeff
    point_20 <- Intercept + time_20 + month_coeff
  }else if (variablemonth == "monthJanuar") {
    point_12 <- Intercept + time_12 + day_coeff
    point_16 <- Intercept + time_16 + day_coeff
    point_20 <- Intercept + time_20 + day_coeff
  }else{
    point_12 <- Intercept + time_12 + month_coeff + day_coeff
    point_16 <- Intercept + time_16 + month_coeff + day_coeff
    point_20 <- Intercept + time_20 + month_coeff + day_coeff
  }
  #Bootstrapping
  residuals <- residuals(model)
  bootstrap_pred <- array(dim = c(B, 3))
  for(i in 1:B) {

    sample_residuals <- sample(residuals, length(residuals), replace = TRUE)
    
   
    bootstrap_pred[i, 1] <- point_12 + sample(sample_residuals, 1)  # Für 12:00
    bootstrap_pred[i, 2] <- point_16 + sample(sample_residuals, 1)  # Für 16:00
    bootstrap_pred[i, 3] <- point_20 + sample(sample_residuals, 1)  # Für 20:00
  }

  pred_seasonal_bootstrap <- apply(bootstrap_pred, 2, function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
  
 
  pred_seasonal <- matrix(unlist(pred_seasonal_bootstrap), nrow = 3, byrow = TRUE)
  rownames(pred_seasonal) <- c("12:00", " 16:00", "20:00")
  colnames(pred_seasonal) <- c("2.5%", "25%", "50%", "75%", "97.5%")
  return (pred_seasonal)
}'

# Archiv: Lineare Regression mit Normalverteilung ------------------------------------

normal_method_seasonal_forecasts <- function (d, model){
  month <- format(d, "%B")
  year <- format(d, "%Y")
  day <- format(d, "%A")
  variableday <- paste0("day", day)
  variablemonth <- paste0("month", month)
  
  # Berechnen der Residuen/ Varianz
  residuals <- residuals(model)
  
  #Koeffizientenwerte rausziehen
  Intercept <- summary(model)$coefficients["(Intercept)", "Estimate"]
  if (variablemonth != "monthJanuar") {
    month_coeff <- summary(model)$coefficients[variablemonth, "Estimate"]
  }
  if (variableday != "dayMontag") {
    day_coeff <- summary(model)$coefficients[variableday, "Estimate"]
  }
  time_12 <- summary(model)$coefficients["Anfang12:00", "Estimate"]
  time_16 <- summary(model)$coefficients["Anfang16:00", "Estimate"]
  time_20 <- summary(model)$coefficients["Anfang20:00", "Estimate"]
  sigma <- summary(model)$sigma
  #Point Forecasts
  if (variablemonth == "monthJanuar" && variableday =="dayMontag"){
    point_12 <- Intercept + time_12 
    point_16 <- Intercept + time_16 
    point_20 <- Intercept + time_20 
  }else if(variableday == "dayMontag"){
    point_12 <- Intercept + time_12 + month_coeff 
    point_16 <- Intercept + time_16 + month_coeff
    point_20 <- Intercept + time_20 + month_coeff
  }else if (variablemonth == "monthJanuar") {
    point_12 <- Intercept + time_12 + day_coeff
    point_16 <- Intercept + time_16 + day_coeff
    point_20 <- Intercept + time_20 + day_coeff
  }else{
    point_12 <- Intercept + time_12 + month_coeff + day_coeff
    point_16 <- Intercept + time_16 + month_coeff + day_coeff
    point_20 <- Intercept + time_20 + month_coeff + day_coeff
  }
  
  #Quantile mittels Normalverteilung bestimmen
  pred_seasonal <- matrix(0, nrow = 3, ncol = 5)
  rownames(pred_seasonal) <- c("12:00", " 16:00", "20:00")
  colnames(pred_seasonal) <- c("2.5%", "25%", "50%", "75%", "97.5%")
  pred_seasonal[1, ] <- qnorm(tau, mean = point_12, sd = sigma)
  pred_seasonal[2, ] <- qnorm(tau, mean = point_16, sd = sigma)
  pred_seasonal[3, ] <- qnorm(tau, mean = point_20, sd = sigma)
  
  return (pred_seasonal)
}

# Verteilungsannahmen prüfen ----------------------------------------------

# seasonal_model <- method_seasonal_model(train_data)
# 
# (ljung_box_resultat <- Box.test(seasonal_model$residuals, type = "Ljung-Box"))
# residuen <- seasonal_model$residuals
# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\ACF_Seasonal Regression.pdf", width = 7, height = 4)
# # ACF Plot
# acf(residuen, main="ACF of the Seasonal Regression Residuals")
# dev.off()
# 
# pdf("C:\\Users\\monam\\PTSFC - Data und Programme\\Plots\\PACF_Seasonal Regression.pdf", width = 7, height = 4)
# # PACF Plot
# pacf(residuen, main="PACF of the Seasonal Regression Residuals")
# dev.off()

