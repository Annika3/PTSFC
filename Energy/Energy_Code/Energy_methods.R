library(dplyr)
library(tidyr)
setwd ("C:\\Users\\monam\\PTSFC - Data und Programme\\R - Data\\Energy")

is_holiday <- function(date) {
  german_holidays <- read.csv("Energy_Daten\\German_Holidays_2000_2024.csv")
  return (date %in% german_holidays$Datum)
}

modify_data <- function (data) {
  data <- subset(data, select = -c(5, 6))#unnötige Spalten entfernen
  data$Datum <- as.Date(data$Datum, format = "%d.%m.%Y")
  data$day <- as.factor(strftime(data$Datum, format = "%A"))#Spalte mit Wochentag hinzufügen
  data$day <- factor(data$day, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
  data$yearday <- as.factor(strftime(data$Datum, format = "%m%d")) #Spalte mit Jahrestag hinzufügen (Z1Z2=Monat Z3Z4=Tag)
  data$month <- as.factor(strftime(data$Datum, format = "%B")) #Spalte mit Monat hinzufügen
  data$month <- factor(data$month, levels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))
  data$year <- as.factor(strftime(data$Datum, format = "%Y")) #Spalte mit Jahr hinzufügen
  data$holiday <- sapply(data$Datum, is_holiday) #Holiday? -> TRUE
  data$Gesamt..Netzlast...MWh..Berechnete.Auflösungen <- gsub("\\.", "", data$Gesamt..Netzlast...MWh..Berechnete.Auflösungen)
  data$Gesamt..Netzlast...MWh..Berechnete.Auflösungen <- as.numeric(gsub(",", ".", data$Gesamt..Netzlast...MWh..Berechnete.Auflösungen))
  data$Gesamt..Netzlast...MWh..Berechnete.Auflösungen <- (data$Gesamt..Netzlast...MWh..Berechnete.Auflösungen / 1000)
  data <- data %>% rename(GWh = Gesamt..Netzlast...MWh..Berechnete.Auflösungen) 
  return(data)
}

get_temp <- function(){
  # library("devtools")
  # install_github("Ram-N/weatherData")
  # library("weatherData")
  #install.packages("rdwd")
  library(rdwd)
  
  station_temp <- selectDWD(res="hourly", var="air_temperature", per="historical", id =1420)
  station_temp_recent <- selectDWD(res="hourly", var="air_temperature", per="recent", id =1420)
  
  data_path_temp <- dataDWD(station_temp, read=FALSE)
  data_path_temp_recent <- dataDWD(station_temp_recent, read=FALSE)
  
  temp_data_historical <- readDWD(data_path_temp, varnames=TRUE)
  temp_data_recent <- readDWD(data_path_temp_recent, varnames=TRUE)
  
  temp_data <- rbind( temp_data_historical,temp_data_recent)
  temp_data <- temp_data %>% select(MESS_DATUM, TT_TU.Lufttemperatur)
  return (temp_data)
}

get_sun <- function(){
  # library("devtools")
  # install_github("Ram-N/weatherData")
  # library("weatherData")
  #install.packages("rdwd")
  library(rdwd)
  
  station_sun <- selectDWD(res="hourly", var="sun", per="historical", id =1420)
  station_sun_recent <- selectDWD(res="hourly", var="sun", per="recent", id =1420)

  data_path_sun <- dataDWD(station_sun, read=FALSE)
  data_path_sun_recent <- dataDWD(station_sun_recent, read=FALSE)

  sun_data_historical <- readDWD(data_path_sun, varnames=TRUE)
  sun_data_recent <- readDWD(data_path_sun_recent, varnames=TRUE)
  
  sun_data <- rbind(sun_data_historical, sun_data_recent)
  sun_data <- sun_data %>% select(MESS_DATUM, SD_SO.Sonnenscheindauer)
  return (sun_data)
}