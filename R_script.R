
library(dplyr) # Para facilitar la mineria de datos

 normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

RMSE = function(e, o){
  sqrt(mean((e - o)^2))
}

	############################################################################################################
################################################## Valor a pagar ##################################################
############################################################################################################

######################## Data ###########################################	
######################## Normalizar la data entre valores desde 0 hasta 1 ###########################################

df <- read.csv('data_supervised.csv')[,-18:-19]%>%
               lapply( normalize)%>%
		as.data.frame() 
            

################################# Guardemos los 600 datos como test final ###################################

input_600 <- df[(nrow(df)-599):nrow(df),]  
input <- df[1:(nrow(df)-600),] 


################################ Mezclemos la data y guardemos una parte para validar #######################

index = sample (1:nrow(input),nrow(input))

train = input[1:(NROW(index)* 0.7),]
valida = input[-1:-(NROW(index)* 0.7),]

# Algoritmo 1

library(randomForest)

set.seed(7)   
rf <-  randomForest(train$DLI_VALOR_TOTAL.x ~.,
data = train[,-train$DLI_VALOR_TOTAL.x])

res_val <- predict (rf,valida)

cor (res_val, valida$DLI_VALOR_TOTAL.x)

RMSE(res_val, valida$DLI_VALOR_TOTAL.x)

#################################### Veamos la data test #######################################

res_test <- predict (rf,input_600)

cor (res_test, input_600$DLI_VALOR_TOTAL.x)

RMSE(res_test, input_600$DLI_VALOR_TOTAL.x)

# [1] 0.1245428



################################### Escojamos un modelo utilizando Akaike ################################## 


library(MASS)  
m = stepAIC(glm(train$DLI_VALOR_TOTAL.x~.,
data = train[,-train$DLI_VALOR_TOTAL.x]))

detach("package:MASS", unload = TRUE)


############ Ejecutar el algoritmo Random Forest #############################################################

set.seed(7)   
rf_AIC <-  randomForest(m$formula,
data = train[,-train$DLI_VALOR_TOTAL.x])
   
res_val <- predict (rf_AIC,valida)

cor (res_val, valida$DLI_VALOR_TOTAL.x)

RMSE(res_val, valida$DLI_VALOR_TOTAL.x)


res_test <- predict (rf_AIC,input_600)

cor (res_test, input_600$DLI_VALOR_TOTAL.x)

RMSE(res_test, input_600$DLI_VALOR_TOTAL.x)


#[1] 0.1244271



##################### Graficar los resultados #################################################################
                
#jpeg('plot_carbon.jpg')
                
#plot(1,1,xlab = 'Días desde creación', ylab = 'Precio escalado ($ COL)')
#points(p_60,col=2)  
#lines(p_60,col=2)  
#lines(input_60$OUTPUT)
                
#dev.off() 









































