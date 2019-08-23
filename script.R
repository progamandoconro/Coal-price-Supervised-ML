###################### Data Cruzada 1 ###############################

	library(dplyr) # Código con arquitectura de tuberías (%>%) 
	library(zoo) # Homogeneizar la cantidad de datos por mes
       	library(lubridate)
	library(dplyr)
	library(stringr)

# Funciones necesarias:

	Normalizar <- function(x) { 
  	return((x - min(x)) / (max(x) - min(x)))
	}

	RMSE = function(esperados, observados){
  	sqrt(mean((esperados - observados)^2))
	}

# Data base

	df = read.csv("precio_carbon.csv")


	fecha <- parse_date_time(df$ENT_FECHA_ENTRADA,
	orders = "%Y-%m-%d %H:%M:%S")

	mes <- month(as.POSIXlt(fecha, format="%Y-%m-%d %H:%M:%S"))

	anio <- year(as.POSIXlt(fecha, format="%Y-%m-%d %H:%M:%S"))

	mes_anio <-paste(anio,mes)

	input = df [, c(-3,-4,-6)]


