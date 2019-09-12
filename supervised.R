library(dplyr)
library(zoo)
library(randomForest)
library(groupdata2)
library(e1071)
library(neuralnet)
library(caret)

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

#test= df_cruz[(nrow(df_cruz)-220):(nrow(df_cruz)),]

###################################################

#df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
set.seed(777)

index <- sample(1:nrow(df_cruz),nrow(df_cruz))

train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]

val <- df_cruz[(floor(nrow(df_cruz)*0.7)+1):nrow(df_cruz),]

rF <- randomForest (as.factor(train$output)~., data=train[,-1], scale=T)
svm <- svm (as.factor(train$output)~., data=train[,-1], scale=T)

p <- predict(svm, val[,-1])
p2 <- predict(rF, val[,-1])




c=vector() 
for (i in c(1:1000)){
set.seed(i)
rF <- randomForest (as.factor(train$output)~., ntree=i ,data=train[,-1], scale=T)
p2 <- predict(rF, val[,-1])
l=confusionMatrix(p2,as.factor(val[,1]))
c[i]= l$overall[1]
}





c=vector() ; t=vector()
for (i in c(1:1000)){
for (e in c(sqrt(20),sqrt(30),sqrt(40),sqrt(ncol(train)),sqrt(100), sqrt(200))){ 
set.seed(777)
rF <- randomForest (as.factor(train$output)~., ntree=i ,mtry=e, data=train[,-1], scale=T)
p2 <- predict(rF, val[,-1])
l=confusionMatrix(p2,as.factor(val[,1]))
c[i]= l$overall[1]
t[e]=l$overall[1]

}
}
