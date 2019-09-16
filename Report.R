rsconnect::setAccountInfo(name='omgonzalez',
			  token='EE20E5653E9A14FA831E3201D17A3438',
			  secret='/5EwCcmRsWrAdNDPKEdZJ8cNvFX+GhHlGUwkH+zl')

#El objetivo de este estudio es la predicción del valor y el peso del Carbón en Termopaipa para los 12 meses futuros. 
#Se intentaron varios algoritmos de aprendizaje automatizado, o Machine Learning (ML) en inglés. Primero se utilizaron los valores numéricos en su naturaleza original, es decir, valores contínuos, variando entre 20 y 40 millones de pesos colombianos y 20 y 40 toneladas. 

#Se utilizó el programa y lenguaje de programación R. A continuación el código empleado, con explicación paso a paso:

#Leemos la data

df=read.csv('carbon_3.csv')

#Debido a la presencia de variables redundante y en formato inadecuado, además de la presencia de valores faltantes, eliminamos variables en formato de fecha y utilizamos la función na.aggregate para generar data que reemplace los valores ausentes. Para especificaciones de como funciona esta función, podemos utilizar help(na.aggregate). 
 
library(zoo)
library(dplyr)

df <- select (df,-starts_with("date"))%>%
  select(-starts_with("date."))%>%
  na.aggregate(by='Anio')%>%
  na.aggregate(by='Mes')%>%
  na.aggregate(by='Dia')%>%
  na.aggregate()

#Ahora, generamos una nueva variable categórica que representa a nuestra unidad temporal de interés, siendo ésta los meses desde el inicio al fin en la data disponible (Oct. 2008 - Abr. 2019), es decir 118 meses.

df$cat= paste(df[,1], df[,2])
df$cat=as.factor(df$cat)
levels(df$cat) <- 1:118
df$cat <- as.numeric(df$cat)

#Si ejecutamos table (df[,1], df[,2]) veremos que la data está desbalanceada con respecto a nuestra unidad temporal (mes en cada año), por lo que utilizamos la función balance del paquete groupdata2 para corregir tal característica. Esto nos permitirá manipular las unidades temporales más comodamente en el próximo paso. Durante el proceso aprovechamos para escalar toda la data a valores contínuos entre 0 y 1 con la siguiente función:  



normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

library (groupdata2)

df <- balance(df,size='max', cat_col='cat')%>%
        lapply(normalize)%>%
        as.data.frame()

#Podemos cerciorar que ahora el número de días en cada mes es homogeneo y hemos balanceado la data. 

table(df[,1],df[,2])

##################################################

#Ahora vamos a cruzar los datos. Esto significa que, utilizando la unidad temporal, desplazaremos un mes las variables independientes con respecto a la dependiente. En otras palabras, ordenamos los datos de forma que, las filas en las variables explicativas del mes anterior quedan paralelas a las filas de la variable target del mes próximo. Con esto se logra generar un modelo predictivo que, con los datos del mes anterior, intenta predecir la variable respuesta del mes próximo. Este proceso se repetirá más adelante para 12 meses siguientes al último registro en data, por ahora comenzamos con un solo mes. 
  
m=1
n=28*m

input<- df

output.1<-df$DLI_PESO_A_PAGAR
output.2<-df$DLI_VALOR_TOTAL

df_cruz.1 <- data.frame(output.1,input) ; df_cruz.2 <- data.frame(output.2,input)

output.1<- vector()
 
for (i in 1:NROW(df_cruz.1[,1])) {

output.1[i]<-ifelse( df_cruz.1[i,1]<df_cruz.1[i+n,1],1,0 )

 }

output.2<- vector()
 
for (i in 1:NROW(df_cruz.2[,1])) {

output.2[i]<-ifelse( df_cruz.2[i,1]<df_cruz.2[i+n,1],1,0 )

 }

#En este punto vamos a reservar una data de evaluación, la cual corresponde a los últimos 10 meses de registro. De esta forma, evaluaremos nuestras predicciones simulando que no teniamos estos datos. En otras palabras, crearemos un modelo sin los datos de los últimos 10 meses para que el error final del algoritmo se base en las predicciones de tales meses. Adicionalmente, aleatorizaremos la data y reserveramos un 30% de datos para la validación del modelo. La sección de validación servirá para afinar las predicciones, ajustando los parámetros de los modelos, así como el ensamble o agrupación de los mismos. 


test.1= df_cruz.1[(nrow(df_cruz.1)-280):(nrow(df_cruz.1)),]

df_cruz.1 <- df_cruz.1[1:(nrow(df_cruz.1)-280),]
set.seed(777)

index <- sample(1:nrow(df_cruz.1),nrow(df_cruz.1))

train.1 <- df_cruz.1[1:floor(nrow(df_cruz.1)*0.7),]

val.1 <- df_cruz.1[(floor(nrow(df_cruz.1)*0.7)+1):nrow(df_cruz.1),]


#Ya podemos crear nuestros primeros modelos y evaluar sus resultados sobre la data de validación. 


library(e1071)
set.seed(777)
svm <- svm (as.factor(train$output)~., data=train[,-1], scale=T)
p <- predict(svm, val[,-1])
confusionMatrix(p,as.factor(val[,1]))


set.seed(777)
library (randomForest)
rF <- randomForest (as.factor(train.1$output.1)~., ntree=500 ,data=train.1[,-1], scale=T)
p2 <- predict(rF, val[,-1])
confusionMatrix(p2,as.factor(val[,1]))


library (neuralnet)
set.seed(777)
net=neuralnet(train$output~., data=train[,-1], hidden=10,linear.output=F,stepmax=10e06)
p3=predict(net,val[,-1])
p3=ifelse(p3>0.5,1,0)
confusionMatrix(as.factor(p3),as.factor(val[,1]))


#Entre Support Vector Machine, Redes Neuronales Artificiales y Random Forest, el últimos se queda con los mejores resultados. Tratemos de mejorar tales resultados ajustando número de árboles a utilizar con el algoritmo random forest. 


rF <- randomForest (as.factor(train.1$output.1)~., ntree=600 ,data=train.1[,-1], scale=T)
p2 <- predict(rF, val[,-1])
confusionMatrix(p2,as.factor(val[,1]))


rF <- randomForest (as.factor(train$output.1)~., ntree=1000 ,data=train[,-1], scale=T)
p2 <- predict(rF, val[,-1])
confusionMatrix(p2,as.factor(val[,1]))



c=vector() ; w=vector()
for (i in c(1:1000)){
set.seed(i)
rF <- randomForest (as.factor(train$output.1)~., ntree=i ,data=train[,-1], scale=T)
p2 <- predict(rF, val[,-1])
l=confusionMatrix(p2,as.factor(val[,1]))
c[i]= l$overall[1]
w[i]=which.max(c)
}



c=vector() ; w=vector()
for (i in c(1:1000)){
for (e in 1:ncol(train)) {

set.seed(777)
rF <- randomForest (as.factor(train.1$output.1)~., ntree=i , mtry=e, data=train.1[,-1], scale=T)
p2 <- predict(rF, val[,-1])
l=confusionMatrix(p2,as.factor(val[,1]))
c[i]= l$overall[1]
w[e]=which.max(c)
}}
