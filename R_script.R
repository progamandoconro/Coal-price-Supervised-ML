# Coal future price prediction.	
# 2019. Rodrigo Diaz-Lupanow (programandoconro). Code for private client.  

	library (dplyr)

	df <- read.csv('data_supervised.csv')
	df$anio_mes <- paste(df[,1],df[,2])

	df$valor <- df$DLI_VALOR_TOTAL.x ; df$peso <- df$DLI_PESO_A_PAGAR.x

	mes_dc <- df$Mes.n + 1 ; mes_dc <- ifelse(mes_dc == 12, 1, mes_dc)
	
	df_2 <- data.frame (df$valor, df$peso, Mes.n=mes_dc)

	bind_rows <- (df,df_2)

	

