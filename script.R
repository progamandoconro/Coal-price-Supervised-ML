         	
#install.packages(c('dplyr','lubridate','ggplot2','shiny','keras','randomForest','e1071','zoo','stringr','openxlsx','groupdata2','shinydashboard'))

	library(openxlsx)
        library(lubridate)
        library(dplyr)
        library(stringr)
        library(groupdata2)
        library(keras)
	library(tensorflow)

# Funciones necesarias:

        normalizar <- function (x) {
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

# Vamos a escojer los meses con mas de 1000 casos para el entrenamiento y el resto par$

train <- df[df$anio>2009 & df$anio<2019 & df$anio != 2017,]%>%
                   balance(size='median', cat_col='unid_tiempo')



test <- df[df$anio<2010 | df$anio>2018 | df$anio==2017,]

#table(df$mes,df$anio)
#table(train$mes,train$anio)

#cruzar datos

m=1 #meses a predecir en el futuro

n=2942*m

input<- train[1:(nrow(train)-n),c(-5,-18)]

output<-train$DLI_PESO_A_PAGAR[(n+1):nrow(train)]


df_cruz <- data.frame(output,input)

df_cruz<- for (i in 1:ncol(df_cruz)){
        
        ifelse(df_cruz[,i]==0,mean(df_cruz[,i],df_cruz)
               
               }


lapply(df_cruz,normalizar) %>% 
			as.data.frame()

set.seed(777)

index <- sample(1:nrow(df_cruz),nrow(df_cruz))




train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]

val <- df_cruz[(floor(nrow(df_cruz)*0.7)+1):nrow(df_cruz),]


#write.csv(df_cruz, 'data_cruzada.csv',row.names=F)
#write.csv(test,'test.csv',row.names=F)


# Predicciones para el mes proximo

model = keras_model_sequential() %>%
   layer_dense(25, activation="relu", input_shape=ncol(train[,-1]))%>%
   layer_dense(units=50, activation = "relu") %>%
   layer_dense(units=1, activation="linear")

model %>% compile(
   loss = "mae",
   optimizer =  "adam",
   metrics = list("mean_absolute_error")
 )

model %>% summary()

model %>% fit(as.matrix(train[,-1]), train[,1], epochs = 10,verbose = 1)

scores = model %>% evaluate(as.matrix(train[,-1]), train[,1], verbose = 1)
print(scores)


p <- predict (model, as.matrix(val[,-1]))

a=RMSE (p,val[,1])

b=cor (p,val[,1])


write.csv(a,'rmse.csv')
write.csv(b,'cor.csv')

png('plot1.png')
plot(p,val[,1])
dev(off)




