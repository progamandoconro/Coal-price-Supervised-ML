        library(zoo) # Homogeneizar la cantidad de datos por mes
        library(openxlsx)
        library(lubridate)
        library(dplyr)
        library(stringr)
	library(groupdata2)
	library(keras)
	library(e1071)
# Funciones necesarias:

        normalizar <- function(x) {
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

input<- train[1:(nrow(train)-2942),c(-5,-18)]
output<-train$DLI_PESO_A_PAGAR[2943:nrow(train)]


df_cruz <- data.frame(output,input)%>%
lapply(normalizar) %>% as.data.frame()


#write.csv(df_cruz, 'data_cruzada.csv',row.names=F)
#write.csv(test,'test.csv',row.names=F)


model = keras_model_sequential() %>% 
   layer_dense(units=ncol(input), activation="relu", input_shape=ncol(input)) %>% 
   layer_dense(units=32, activation = "relu") %>% 
   layer_dense(units=1, activation="linear")
 
model %>% compile(
   loss = "mse",
   optimizer =  "adam", 
   metrics = list("mean_absolute_error")
 )
 
model %>% summary()


model %>% fit(as.matrix(df_cruz[,-1]), df_cruz[,1], epochs = 100,verbose = 0)
 
#scores = model %>% evaluate(df_cruz[,-1], df_cruz[,1], verbose = 0)
#print(scores)



