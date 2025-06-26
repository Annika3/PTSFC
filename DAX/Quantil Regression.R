
library(dplyr)
library(lubridate)
library(quantreg)
library(ggplot2)

setwd("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data") #working directory
source("DAX\\dax_procs.R")
source("DAX\\Evaluation.R")
tau <- c(.025, .25, .5, .75, .975) # quantile levels


# DAX Werte importieren, Returns berechnen -----------------------------

dax_data = get.hist.quote(instrument="^GDAXI", start="2010-01-01",
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



dat_returns <- dat_returns %>% filter(format(Datum, "%Y") != "2020")
# Lags für jeden Horizont erstellen + Train/Val/Test ---------------------------------------

dax_r1 <- dat_returns %>% select("Datum", "ret1")
dax_r2 <- dat_returns %>% select("Datum", "ret2")
dax_r3 <- dat_returns %>% select("Datum", "ret3")
dax_r4 <- dat_returns %>% select("Datum", "ret4")
dax_r5 <- dat_returns %>% select("Datum", "ret5")

n_lags = 10  
for (i in 1:n_lags) {
  lag_name = paste0('lag_', i)
  dax_r1[[lag_name]] = lag(dax_r1$ret1, n = i)
}
for (i in 1:n_lags) {
  lag_name = paste0('lag_', i)
  dax_r2[[lag_name]] = lag(dax_r2$ret2, n = i)
}
for (i in 1:n_lags) {
  lag_name = paste0('lag_', i)
  dax_r3[[lag_name]] = lag(dax_r3$ret3, n = i)
}
for (i in 1:n_lags) {
  lag_name = paste0('lag_', i)
  dax_r4[[lag_name]] = lag(dax_r4$ret4, n = i)
}
for (i in 1:n_lags) {
  lag_name = paste0('lag_', i)
  dax_r5[[lag_name]] = lag(dax_r5$ret5, n = i)
}


#Unterteilung in Train, Validation und Testset
dax_r5 <- na.omit(dax_r5)
n <- nrow(dax_r5)
dax_r1 <- tail(dax_r1, n)
dax_r2 <- tail(dax_r2, n)
dax_r3 <- tail(dax_r3, n)
dax_r4 <- tail(dax_r4, n)

n_train <- floor(0.7 * n)
n_val <- floor(0.1 * n) + n_train

dax_r1_train <- dax_r1[1:n_train, ]
dax_r1_val <- dax_r1[(n_train + 1):n_val, ]
dax_r1_test <- dax_r1[(n_val + 1):n, ]

dax_r2_train <- dax_r2[1:n_train, ]
dax_r2_val <- dax_r2[(n_train + 1):n_val, ]
dax_r2_test <- dax_r2[(n_val + 1):n, ]

dax_r3_train <- dax_r3[1:n_train, ]
dax_r3_val <- dax_r3[(n_train + 1):n_val, ]
dax_r3_test <- dax_r3[(n_val + 1):n, ]

dax_r4_train <- dax_r4[1:n_train, ]
dax_r4_val <- dax_r4[(n_train + 1):n_val, ]
dax_r4_test <- dax_r4[(n_val + 1):n, ]

dax_r5_train <- dax_r5[1:n_train, ]
dax_r5_val <- dax_r5[(n_train + 1):n_val, ]
dax_r5_test <- dax_r5[(n_val + 1):n, ]

# Modelle schätzen --------------------------------------------------------

method_dax_quantile_1 <- function (train_data, tau_level){
  model <- rq(train_data[,2] ~ lag_1   , data = train_data, tau = tau_level)
  return (model)
}
method_dax_quantile_2 <- function (train_data, tau_level){
  model <- rq(train_data[,2] ~ lag_1   , data = train_data, tau = tau_level)
  return (model)
}
method_dax_quantile_3 <- function (train_data, tau_level){
  model <- rq(train_data[,2] ~ lag_1   , data = train_data, tau = tau_level)
  return (model)
}
method_dax_quantile_4 <- function (train_data, tau_level){
  model <- rq(train_data[,2] ~ lag_1 , data = train_data, tau = tau_level)
  return (model)
}
method_dax_quantile_5 <- function (train_data, tau_level){
  model <- rq(train_data[,2] ~ lag_1 , data = train_data, tau = tau_level)
  return (model)
}
dax_r1_list <- list(
  "0.025" = method_dax_quantile_1(dax_r1_train, 0.025),
  "0.25" = method_dax_quantile_1(dax_r1_train, 0.25),
  "0.5" = method_dax_quantile_1(dax_r1_train, 0.5),
  "0.75" = method_dax_quantile_1(dax_r1_train, 0.75),
  "0.975" = method_dax_quantile_1(dax_r1_train, 0.975)
)
dax_r2_list <- list(
  "0.025" = method_dax_quantile_2(dax_r2_train, 0.025),
  "0.25" = method_dax_quantile_2(dax_r2_train, 0.25),
  "0.5" = method_dax_quantile_2(dax_r2_train, 0.5),
  "0.75" = method_dax_quantile_2(dax_r2_train, 0.75),
  "0.975" = method_dax_quantile_2(dax_r2_train, 0.975)
)
dax_r3_list <- list(
  "0.025" = method_dax_quantile_3(dax_r3_train, 0.025),
  "0.25" = method_dax_quantile_3(dax_r3_train, 0.25),
  "0.5" = method_dax_quantile_3(dax_r3_train, 0.5),
  "0.75" = method_dax_quantile_3(dax_r3_train, 0.75),
  "0.975" = method_dax_quantile_3(dax_r3_train, 0.975)
)
dax_r4_list <- list(
  "0.025" = method_dax_quantile_4(dax_r4_train, 0.025),
  "0.25" = method_dax_quantile_4(dax_r4_train, 0.25),
  "0.5" = method_dax_quantile_4(dax_r4_train, 0.5),
  "0.75" = method_dax_quantile_4(dax_r4_train, 0.75),
  "0.975" = method_dax_quantile_4(dax_r4_train, 0.975)
)
dax_r5_list <- list(
  "0.025" = method_dax_quantile_5(dax_r5_train, 0.025),
  "0.25" = method_dax_quantile_5(dax_r5_train, 0.25),
  "0.5" = method_dax_quantile_5(dax_r5_train, 0.5),
  "0.75" = method_dax_quantile_5(dax_r5_train, 0.75),
  "0.975" = method_dax_quantile_5(dax_r5_train, 0.975)
)


# Forecasts generieren ----------------------------------------------------

dax_r1_quantreg_forecasts <- function (day, data){
  
  datums_index <- which(data$Datum == day)
  day_r1 <- datums_index +1
  new_data <- data[day_r1,]
  pred_ret <- rep(NA, times=5)
  for (i in tau){
    string_tau <- as.character(i)
    model <- dax_r1_list[[string_tau]]
    point <- predict(model, new_data)
    index <- match(i, tau)
    pred_ret[index] <- point
  }
  return (pred_ret)
}

dax_r2_quantreg_forecasts <- function (day, data){
  
  datums_index <- which(data$Datum == day)
  day_r2 <- datums_index +2
  new_data <- data[day_r2,]
  pred_ret <- matrix(nrow = 2, ncol = 5)
  
  for(j in 1:2){
    for(i in tau){
    string_tau <- as.character(i)
    model <- dax_r2_list[[string_tau]]
    point <- predict(model, new_data)
    index <- match(i, tau)
    pred_ret[j, index] <- point
    }
    new_data$lag_5 <- new_data$lag_4
    new_data$lag_4 <- new_data$lag_3
    new_data$lag_3 <- new_data$lag_2
    new_data$lag_2 <- new_data$lag_1
    new_data$lag_1 <- pred_ret[j, 3]
  }
  return (pred_ret[2, ])
}
dax_r3_quantreg_forecasts <- function (day, data){
  
  datums_index <- which(data$Datum == day)
  day_r3 <- datums_index +3
  new_data <- data[day_r3,]
  pred_ret <- matrix(nrow = 3, ncol = 5)
  
  for(j in 1:3){
    for(i in tau){
      string_tau <- as.character(i)
      model <- dax_r3_list[[string_tau]]
      point <- predict(model, new_data)
      index <- match(i, tau)
      pred_ret[j, index] <- point
    }
    new_data$lag_5 <- new_data$lag_4
    new_data$lag_4 <- new_data$lag_3
    new_data$lag_3 <- new_data$lag_2
    new_data$lag_2 <- new_data$lag_1
    new_data$lag_1 <- pred_ret[j, 3]
  }
  return (pred_ret[3, ])
}
dax_r4_quantreg_forecasts <- function (day, data){
  
  datums_index <- which(data$Datum == day)
  day_r4 <- datums_index +4
  new_data <- data[day_r4,]
  pred_ret <- matrix(nrow = 4, ncol = 5)
  
  for(j in 1:4){
    for(i in tau){
      string_tau <- as.character(i)
      model <- dax_r4_list[[string_tau]]
      point <- predict(model, new_data)
      index <- match(i, tau)
      pred_ret[j, index] <- point
    }
    new_data$lag_5 <- new_data$lag_4
    new_data$lag_4 <- new_data$lag_3
    new_data$lag_3 <- new_data$lag_2
    new_data$lag_2 <- new_data$lag_1
    new_data$lag_1 <- pred_ret[j, 3]
  }
  return (pred_ret[4, ])
}
dax_r5_quantreg_forecasts <- function (day, data){
  
  datums_index <- which(data$Datum == day)
  day_r5 <- datums_index +5
  new_data <- data[day_r5,]
  pred_ret <- matrix(nrow = 5, ncol = 5)
  
  for(j in 1:5){
    for(i in tau){
      string_tau <- as.character(i)
      model <- dax_r5_list[[string_tau]]
      point <- predict(model, new_data)
      index <- match(i, tau)
      pred_ret[j, index] <- point
    }
    new_data$lag_5 <- new_data$lag_4
    new_data$lag_4 <- new_data$lag_3
    new_data$lag_3 <- new_data$lag_2
    new_data$lag_2 <- new_data$lag_1
    new_data$lag_1 <- pred_ret[j, 3]
  }
  return (pred_ret[5, ])
}


# Evaluation + Forecasts generieren  -----------------------------------------
qr_evaluation_matrix <- dax_r1_val
qr_evaluation_matrix$QS <- NA

for(i in 1: (nrow(dax_r1_val) - 5)) {
  print (i/ nrow(dax_r1_val)) # Fortschrittsanzeige
  datum <- dax_r1_val$Datum[i]
  
  for_r1 <- dax_r1_quantreg_forecasts (day = datum, data = dax_r1_val)
  for_r2 <- dax_r2_quantreg_forecasts (day = datum, data = dax_r2_val)
  for_r3 <- dax_r3_quantreg_forecasts (day = datum, data = dax_r3_val)
  for_r4 <- dax_r4_quantreg_forecasts (day = datum, data = dax_r4_val)
  for_r5 <- dax_r5_quantreg_forecasts (day = datum, data = dax_r5_val)

  forecasts <- rbind(for_r1, for_r2, for_r3, for_r4, for_r5)
  colnames(forecasts) <- c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  qs_score <- evaluation(datum, data=dat_returns, forecasts=forecasts)
  print(qs_score)
  qr_evaluation_matrix$QS[i] <- qs_score
}
val <- qr_evaluation_matrix %>% filter(QS > 1)
tail(qr_evaluation_matrix)
(mean(qr_evaluation_matrix$QS, na.rm = TRUE))


# forc1 <- seasonal_forecasts(as.Date("xx"), data = dax_r1)
# forc2 <- seasonal_forecasts(as.Date("xx"), data = dax_r2)
# forc3 <- seasonal_forecasts(as.Date("xx"), data = dax_r3)
# forc4 <- seasonal_forecasts(as.Date("2x"), data = dax_r4)
# forc5 <- seasonal_forecasts(as.Date("xx"), data = dax_r5)

