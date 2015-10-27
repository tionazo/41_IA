###Crear una matriz 10x10 a partir de una secuencia de números de 1 a 100. 
	#Para poder tener resultados en los siguientes ejercicios, creamos dos matrices, una por filas y otra por columnas

matriz.10.10.f <- matrix( nrow = 10, ncol=10, data=1:100, byrow=T)
matriz.10.10.f

matriz.10.10.c <- matrix( nrow = 10, ncol=10, data=1:100)
matriz.10.10.c 



#Seleccionar las filas de la matriz cuyo mínimo sea mayor que 20. 
	#usaremos la matriz creada por filas

mat.fila.min.mayor20.matriz.10.10 <- matriz.10.10[ apply( matriz.10.10, MARGIN=1, FUN='min') > 20]

mat.fila.min.mayor20.matriz.10.10



#Seleccionar las columnas de la matriz cuya suma sea mayor que 80. 

mat.cols.sum.mayor80.matriz.10.10 <- matriz.10.10[ apply( matriz.10.10, MARGIN=2, FUN='sum') > 80]

mat.cols.sum.mayor80.matriz.10.10




#Seleccionar las columnas de la matriz cuyo máximo sea mayor que 50
