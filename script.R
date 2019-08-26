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


# Predicciones para el mes proximo

model = keras_model_sequential() %>% 
   layer_dense(units=ncol(input), activation="relu", input_shape=ncol(input)) %>% 
   layer_dense(units=5, activation = "relu") %>% 
   layer_dense(units=1, activation="linear")
 
model %>% compile(
   loss = "mse",
   optimizer =  "adam", 
   metrics = list("mean_absolute_error")
 )
 
model %>% summary()


model %>% fit(as.matrix(df_cruz[,-1]), df_cruz[,1], epochs = 100,verbose = 0)
 
scores = model %>% evaluate(as.matrix(df_cruz[,-1]), df_cruz[,1], verbose = 0)
print(scores)

#3 meses

input<- train[1:(nrow(train)-8826),c(-5,-18)]
output<-train$DLI_PESO_A_PAGAR[8827:nrow(train)]


df_cruz <- data.frame(output,input)%>%
lapply(normalizar) %>% as.data.frame()


#write.csv(df_cruz, 'data_cruzada.csv',row.names=F)
#write.csv(test,'test.csv',row.names=F)


model2 = keras_model_sequential() %>% 
   layer_dense(units=ncol(input), activation="relu", input_shape=ncol(input)) %>% 
   layer_dense(units=5, activation = "relu") %>% 
   layer_dense(units=1, activation="linear")
 
model2 %>% compile(
   loss = "mse",
   optimizer =  "adam", 
   metrics = list("mean_absolute_error")
 )
 
model2 %>% summary()


model2 %>% fit(as.matrix(df_cruz[,-1]), df_cruz[,1], epochs = 100,verbose = 0)
 
scores2 = model2 %>% evaluate(as.matrix(df_cruz[,-1]), df_cruz[,1], verbose = 0)
print(scores2)

#6 meses

input<- train[1:(nrow(train)-17652),c(-5,-18)]
output<-train$DLI_PESO_A_PAGAR[17653:nrow(train)]


df_cruz <- data.frame(output,input)%>%
lapply(normalizar) %>% as.data.frame()


#write.csv(df_cruz, 'data_cruzada.csv',row.names=F)
#write.csv(test,'test.csv',row.names=F)


model3 = keras_model_sequential() %>% 
   layer_dense(units=ncol(input), activation="relu", input_shape=ncol(input)) %>% 
   layer_dense(units=5, activation = "relu") %>% 
   layer_dense(units=1, activation="linear")
 
model3 %>% compile(
   loss = "mse",
   optimizer =  "adam", 
   metrics = list("mean_absolute_error")
 )
 
model3 %>% summary()


model3 %>% fit(as.matrix(df_cruz[,-1]), df_cruz[,1], epochs = 100,verbose = 0)
 
scores3 = model3 %>% evaluate(as.matrix(df_cruz[,-1]), df_cruz[,1], verbose = 0)
print(scores3)

#12 meses

input<- train[1:(nrow(train)-35304),c(-5,-18)]
output<-train$DLI_PESO_A_PAGAR[35305:nrow(train)]


df_cruz <- data.frame(output,input)%>%
lapply(normalizar) %>% as.data.frame()


#write.csv(df_cruz, 'data_cruzada.csv',row.names=F)
#write.csv(test,'test.csv',row.names=F)


model4 = keras_model_sequential() %>% 
   layer_dense(units=ncol(input), activation="relu", input_shape=ncol(input)) %>% 
   layer_dense(units=5, activation = "relu") %>% 
   layer_dense(units=1, activation="linear")
 
model4 %>% compile(
   loss = "mse",
   optimizer =  "adam", 
   metrics = list("mean_absolute_error")
 )
 
model4 %>% summary()


model4 %>% fit(as.matrix(df_cruz[,-1]), df_cruz[,1], epochs = 100,verbose = 0)
 
scores4 = model4 %>% evaluate(as.matrix(df_cruz[,-1]), df_cruz[,1], verbose = 0)
print(scores4)




############ 
scores
scores2
scores3

