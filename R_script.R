# Coal future price prediction.	
# 2019. Rodrigo Diaz-Lupanow (programandoconro). Code for private client.  


	df <- read.csv('data_supervised.csv')
	df$anio_mes <- paste(df[,1],df[,2])

	valor <- df$DLI_VALOR_TOTAL.x ; peso <- df$DLI_PESO_A_PAGAR.x

	mes_dc <- df$Mes.n + 1 ; mes_dc <- ifelse(mes_dc == 12, 1, mes_dc)
	
	df_2 <- data.frame (valor, peso, Mes.n=mes_dc)

	bind_rows <- (df,df_2)

	

