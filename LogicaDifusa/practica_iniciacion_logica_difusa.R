##Enunciado: 
## 	practica_iniciacion_logica_difusa.pdf
##
## Introducción Sistemas Difusos
## 1. Sea la variable temperatura de valores linguísticos { Baja, Media, Alta } y cuyas funciones de pertenencia se muestran en la figura. Obtener las ecuaciones que modelan dichos conjuntos difusos. Visualizar utilizando R las funciones de pertenencia de los conjuntos difusos Baja y Media, Media o Alta y no Media y Alta. 
	
## 	Programar una función en R que reciba como entradas los parámetros de los conjuntos difusos y que devuelva las funciones de pertenencia y las visualice. Programar otra función que reciba como entradas las funciones de pertenencia de los conjuntos difusos y que realice las operaciones de intersección, unión y complementación.



## Creamos el array
T <- seq(0, 35, by=0.1)
T

length(T)


###############################################################################
#### Función de pertenencia temperatura ALTA  							   ####
###############################################################################

## Creamos un array de tamaño igual al de T que contendrá la discretización del array T
T.baja <- rep(0, length(T) )

## Fijamos a 1 todos aquellos valores menores que 12 
T.baja[ T <= 12 ] <- 1

## Asignamos valores proporcionales que van disminuyendo a partir de la temperatura 12º hasta la temperatura <20
T.baja[ T > 12 & T < 20 ] <- ( 20 - T[T > 12 & T < 20] ) / (20-12)

## Dibujamos la gráfica de la función de pertenencia Baja
plot( T, T.baja, col="blue", type="h", xlab="temperatura", ylab="y", main="Función de pertenencia BAJA")

###############################################################################
#### Función de pertenencia temperatura MEDIA  							   ####
###############################################################################

## Creamos un array de tamaño igual al de T que contendrá la discretización del array T
T.media <- rep(0, length(T) )

## Fijamos a 1 todos aquellos valores menores que 12 
#T.media[ T >=17 & T >= 28 ] <- 1

## Subida
T.media[ T >= 17 & T <= 23 ] <- ( T[T >= 17 & T <= 23] - 17 ) / (23-17)

## Bajada
T.media[ T >= 23 & T <= 28 ] <- ( 28 - T[ T >= 23 & T <= 28 ] ) / (28-23)


## Dibujamos la gráfica de la función de pertenencia Baja
plot( T, T.media, col="green",  type="h", xlab="temperatura", ylab="y", main="Función de pertenencia MEDIA")


###############################################################################
#### Función de pertenencia temperatura ALTA  							   ####
###############################################################################

## Creamos un array de tamaño igual al de T que contendrá la discretización del array T
T.alta <- rep(0, length(T) )

## Fijamos a 1 todos aquellos valores menores que 12 
T.alta[ T >=26 ] <- 1

## 
T.alta[ T >= 26 & T <= 29 ] <- ( T[T >= 26 & T <= 29] - 26 ) / (29-26)


## Dibujamos la gráfica de la función de pertenencia Baja
plot( T, T.alta, col="red", type="h", xlab="temperatura", ylab="y", main="Función de pertenencia ALTA")



## Ploteamos todos los gráficos juntos
plot( T.baja, ylim=range(T.baja, T.media, T.alta), col="blue", type="l")
lines(T.media, col="green")
lines(T.alta,  col="red")

leyenda.grafico.temperatura <- legend("topright", legend = c("Baja ", "Media ", "Alta"), text.col=c( "blue", "green", "red") )



func.devuelve.funcPertenencia <- function( tipo, arrDatos, b, m, a ) {
	##	Esta función recibe los parametros necesarios y devuelve otra funcion con la 
	##		configuración correspondiente. 
	##	Parámetros de la función
	##		tipo: Se refiere al tipo de función
	##			"tri"	para triangular 
	##			"tra"	para trapezoidal 
	##			"gau"	para gaussiana
	##
	##		arrDatos: Array que contiene los datos 
	##
	##		b:  limite inferior 
	##
	##		m: punto de inflexion de la funcion 
	##
	##		a:  limite superior 
	##

	arr.pertenencia <- rep (0, length( arrDatos ) )	# creamos un array de tamaño igual al original relleno con 0's

	if( tipo == "tri" )	{					## TRIANGULAR
		# asignamos datos a la parte ascendente			   
		arr.pertenencia[ arrDatos >= b & arrDatos <= m ] <- ( arrDatos[arrDatos >= b & arrDatos <= m ] - b ) / (m - b)
		# asignamos datos a la parte descendente			   
		arr.pertenencia[ arrDatos >= m & arrDatos <= a ] <- ( a - arrDatos[ arrDatos >= m & arrDatos <= a ] ) / ( a - m )
	} 
	else {
		if( tipo == "tra" ) { 
		## TRAPEZOIDAL
			## TRAPEZOIDAL DESCENDENTE
			if( a < b ) { 
				# cima 
				arr.pertenencia[ arrDatos < m] <- 1
				# descenso 
				arr.pertenencia[ arrDatos >= m & arrDatos <= a ] <- ( a - arrDatos[ arrDatos >= m & arrDatos <= a ] ) / ( a - m )
			} 
			## TRAPEZOIDAL ASCENDENTE 
			else {
				# ascenso 
				arr.pertenencia[ arrDatos >= b & arrDatos <= m ] <- ( arrDatos[arrDatos >= b & arrDatos <= m ] - b ) / (m - b)
				# cima 
				arr.pertenencia[ arrDatos > a] <- 1
			}
		}
		## GAUSSIANA
		else ( tipo == "gau" ) {				
			## pendiente de desarrollar
			notaTexto <- "Esta parte de la función está pendiente de desarrollar"
			notaTexto
		}
	}

	# devolvemos el array de pertenencia
	arr.pertenencia
}






