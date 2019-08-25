###################### Data Cruzada 1 ###############################

	library(dplyr) # Código con arquitectura de tuberías (%>%) 
	library(zoo) # Homogeneizar la cantidad de datos por mes
       	library(openxlsx)
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

download.file(
'https://programandoconro.files.wordpress.com/2019/08/carbon_colombia.xlsx',
destfile='precio_carbon.xlsx')

df=read.xlsx('precio_carbon.xlsx')

fecha <- parse_date_time(df$ENT_FECHA_ENTRADA,orders = "%Y%m%d%H%M%S")
df$anio <- year(as.POSIXlt(fecha, format="%Y-%m-%d %H:%M:%S"))
df$mes <- month(as.POSIXlt(fecha, format="%Y-%m-%d %H:%M:%S"))
unid_tiempo <-paste(df$anio,df$mes)

	input = df [, c(-3,-4,-6)]


