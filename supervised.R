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

confusionMatrix(p,as.factor(val[,1]))
confusionMatrix(p2,as.factor(val[,1]))

net=neuralnet(train$output~., data=train[,-1], hidden=ncol(train)-1,linear.output=F,stepmax=10e06)

p3=predict(net,val[,-1])

p3=ifelse(p3>0.5,1,0)

confusionMatrix(as.factor(p3),as.factor(val[,1]))


