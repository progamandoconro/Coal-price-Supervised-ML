# Precio Futuro del .	
# 2019. Rodrigo Diaz-Lupanow (programandoconro). Code for private client.  

library(dplyr) # Para facilitar la mineria de datos
library(zoo) # Tratamiento para los datos faltantes
library(lubridate) # Tratamiento para las fechas en la data

 normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

	
	

	############################################################################################################
################################################## Valor a pagar ##################################################
############################################################################################################

######################## Data ###########################################	
######################## Normalizar la data entre valores desde 0 hasta 1 ###########################################

input <- read.csv('data_supervised.csv')[,-18:-19]%>%
               lapply( normalize)%>%
		as.data.frame() 
            

################################# Guardemos los 600 datos como test final ###################################

input_600 <- input[(nrow(input)-600):nrow(input),]  
input <- input[1:(nrow(input)-601),] 


################################ Mezclemos la data y guardemos una parte para validar #######################

index = sample (1:nrow(input),nrow(input))

train = input[1:(NROW(index)* 0.7),]
valida = input[-1:-(NROW(index)* 0.7),]

# Algoritmo 1

library(randomForest)

set.seed(7)   
rf <-  randomForest(train$DLI_VALOR_TOTAL.x ~.,data = train[,-train$DLI_VALOR_TOTAL.x])

res_val <- predict (rf,valida)

cor (res_val, valida$DLI_VALOR_TOTAL.x)

#################################### Veamos la data test #######################################

################################### Escojamos un modelo utilizando Akaike ################################## 


#library(MASS)  
#m = stepAIC(glm(d_train$OUTPUT~.,data = d_train[,-d_train$OUTPUT]))

#detach("package:MASS", unload = TRUE)


############ Ejecutar el algoritmo Random Forest #############################################################

set.seed(7)   
#rf <-  randomForest(m$formula,data = d_train[,-d_train$OUTPUT])
              
#p_60 <- predict(rf, input_60[,colnames(input[,-input$OUTPUT]) ])
##################### Graficar los resultados #################################################################
                
#jpeg('plot_carbon.jpg')
                
#plot(1,1,xlab = 'Días desde creación', ylab = 'Precio escalado ($ COL)')
#points(p_60,col=2)  
#lines(p_60,col=2)  
#lines(input_60$OUTPUT)
                
#dev.off() 
	

