        library(zoo) 
	library(openxlsx)
        library(lubridate)
        library(dplyr)
        library(stringr)
	library(groupdata2)

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
df$unid_tiempo <-paste(df$anio,df$mes)

        df = df [, c(-3,-4,-6)]

#table (df$anio,df$mes)

# Vamos a escojer los meses con mas de 1000 casos para el entrenamiento y el resto para validar y test final

train <- df[df$anio>2009 & df$anio<2019 & df$anio != 2017,]%>%
                   balance(size='median', cat_col='unid_tiempo')



test <- df[df$anio<2010 | df$anio>2018 | df$anio==2017,]

#table(df$mes,df$anio)
#table(train$mes,train$anio)

#cruzar datos

df_cruz <- data.frame(train$DLI_PESO_A_PAGAR[2943:nrow(train)], train[1:(nrow(train)-2942),-19] )

write.csv(df_cruz, 'data_cruzada.csv',row.names=F)




