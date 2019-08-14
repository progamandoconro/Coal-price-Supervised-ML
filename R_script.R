# Coal future price prediction.	
# 2019. Rodrigo Diaz-Lupanow (programandoconro). Code for private client.  


	df <- read.csv('data_supervised.csv')

	precio <- df$DLI_PRECIO_A_PAGAR.x ; valor <- df$DLI_VALOR_TOTAL.x ; peso <- df$DLI_PESO_A_PAGAR.x

	mes <- df$mes.n - 1 ; mes <- ifelse(mes == 0, 12, mes)

	dat_cruzada <- data.frame(mes,peso,valor,precio)

  

