# Coal future price prediction.	
# 2019. Rodrigo Diaz-Lupanow (programandoconro). Code for private client.  

	library (dplyr)

	input <- read.csv('data_supervised.csv')
	

	############################################################################################################
################################################## Litecoin ##################################################
############################################################################################################

library(dplyr) # Para facilitar la mineria de datos
library(zoo) # Tratamiento para los datos faltantes
library(lubridate) # Tratamiento para las fechas en la data

######################## Normalizar la data entre valores desde 0 hasta 1 ###########################################
 

                normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
#input<- lapply(input, normalize)%>%
 # as.data.frame()

##input_60 <- input[(nrow(input)-60):nrow(input),]  
#input <- input[1:(nrow(input)-61),] 

#library(MASS)  
#m = stepAIC(glm(d_train$OUTPUT~.,data = d_train[,-d_train$OUTPUT]))

#detach("package:MASS", unload = TRUE)


############ Ejecutar el algoritmo Random Forest #############################################################

library(randomForest)

set.seed(7)   
#rf <-  randomForest(formula,data = d_train[,-d_train$OUTPUT])
              
#p_60 <- predict(rf, input_60[,colnames(input[,-input$OUTPUT]) ])
##################### Graficar los resultados #################################################################
                
jpeg('plot_carbon.jpg')
                
plot(1,1,xlab = 'Días desde creación', ylab = 'Precio escalado ($ COL)')
#points(p_60,col=2)  
#lines(p_60,col=2)  
#lines(input_60$OUTPUT)
                
dev.off() 
	

