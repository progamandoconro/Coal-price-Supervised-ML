# Coal future price prediction.	
# 2019. Rodrigo Diaz-Lupanow (programandoconro). Code for private client.  


	df <- read.csv('data_supervised.csv')
	df$anio_mes <- paste(df[,1],df[,2])
	
	library(ROSE)
	
	df <- ROSE(df$anio_mes~., data=df)

	valor <- df$DLI_VALOR_TOTAL.x ; peso <- df$DLI_PESO_A_PAGAR.x

	mes <- df$Mes.n + 1 ; mes <- ifelse(mes == 12, 1, mes)



