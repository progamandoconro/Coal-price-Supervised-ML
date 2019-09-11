library(dplyr)
library(zoo)
library(randomForest)
library(groupdata2)

RMSE = function(esperados, observados){
sqrt(mean((esperados - observados)^2))
} 

df=read.csv('carbon_3.csv')

df <- select (df,-starts_with("date"))%>%
  select(-starts_with("date."))%>%
  na.aggregate(by='Anio')%>%
  na.aggregate(by='Mes')%>%
  na.aggregate(by='Dia')%>%
  na.aggregate()

df$cat= paste(df[,1], df[,2])
df$cat=as.factor(df$cat)
levels(df$cat) <- 1:118
df$cat <- as.numeric(df$cat)

df <- balance(df,size='max', cat_col='cat')

table(df[,1],df[,2])

m=1
n=28*m

input<- df[1:(nrow(df)-n),]

output<-df$DLI_PESO_A_PAGAR[(n+1):nrow(df)]

df_cruz <- data.frame(output,input)

test= df_cruz[(nrow(df_cruz)-220):(nrow(df_cruz)),]

df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
set.seed(777)

index <- sample(1:nrow(df_cruz),nrow(df_cruz))

train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]

val <- df_cruz[(floor(nrow(df_cruz)*0.7)+1):nrow(df_cruz),]

rF <- randomForest (train$DLI_PESO_A_PAGAR.x~., data=train, scale=T)

p <- predict(rF, val[,-12])

cor(p,val[,12])
RMSE(p,val[,12])
plot(p,val[,12])

p <- predict(rF, test)

plot(test[,12],xlab='Registros diarios más recientes',ylab='Peso a pagar (TON)',ylim=c(16,30))
lines(test[,12])
lines(p,col=2)
points(p,col=2)

######################

df=read.csv('carbon_3.csv')


df <- select (df,-starts_with("date"))%>%
  select(-starts_with("date."))%>%
  na.aggregate(by='Anio')%>%
  na.aggregate(by='Mes')%>%
  na.aggregate(by='Dia')%>%
  na.aggregate()

df$cat= paste(df[,1], df[,2])
df$cat=as.factor(df$cat)
levels(df$cat) <- 1:118
df$cat <- as.numeric(df$cat)



df <- balance(df,size='max', cat_col='cat')

table(df[,1],df[,2])

m=1
n=28*m

input<- df[1:(nrow(df)-n),]

output<-df$DLI_VALOR_TOTAL[(n+1):nrow(df)]

df_cruz <- data.frame(output,input)

test= df_cruz[(nrow(df_cruz)-220):(nrow(df_cruz)),]

df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
set.seed(777)

index <- sample(1:nrow(df_cruz),nrow(df_cruz))

train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]

val <- df_cruz[(floor(nrow(df_cruz)*0.7)+1):nrow(df_cruz),]

rF <- randomForest (train$DLI_VALOR_TOTAL.x~., data=train, scale=T)

p <- predict(rF, val)

cor(p,val[,12])
RMSE(p,val[,12])
plot(p,val[,12])

p <- predict(rF, test)

plot(test$DLI_VALOR_TOTAL.x,xlab='Registros diarios más recientes (Desconocidos para el algoritmo)',ylab='Valor a pagar ($COL)',ylim=c(2000000,3600000))
lines(test$DLI_VALOR_TOTAL.x)
lines(p+400000,col=2)
points(p+400000,col=2)
