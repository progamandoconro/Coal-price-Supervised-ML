library(dplyr)
library(zoo)
library(randomForest)
library(groupdata2)
library(e1071)

RMSE = function(esperados, observados){
sqrt(mean((esperados - observados)^2))
} 


normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
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

df <- balance(df,size='max', cat_col='cat')%>%
	lapply(normalize)%>%
	as.data.frame()


table(df[,1],df[,2])

##################################################
m=1
n=28*m

input<- df

output<-df$DLI_PESO_A_PAGAR

df_cruz <- data.frame(output,input)

output<- vector()
 
for (i in 1:NROW(df_cruz[,1])) {
 
output[i]<-ifelse( df_cruz[i,1]<df_cruz[i+n,1],1,0 )

 }

df_cruz$output<- output

df_cruz<-df_cruz[1:(nrow(df_cruz)-n),]

