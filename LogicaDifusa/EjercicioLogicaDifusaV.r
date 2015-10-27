#Ejercicio entregado en clase sobre lógica difura pero ahora en R

T <- seq(0, 35, by=0.1)
Tbaja <- rep(0, length(T))
Tbaja[ T <= 13 ] <- 1
Tbaja[T > 13 & T < 20] <- (20 - T[ T > 13 & T < 20 ])/(20-13)
plot(T, Tbaja, col="red", xlab="x", ylab="y", main="Funciones pertenencia temperatura baja")

Tmedia <- rep(0, length(T))
Tmedia[T >= 17 & T < 23] <- (T[T >= 17 & T<23] - 17)/(23-17)
Tmedia[T >= 23 & T < 27] <- (27 - T[T >= 23 & T < 27])/(27-23)
plot(T, Tmedia, col="green", xlab="x", ylab="y", main="Funciones pertenencia temperatura media")

Talta <- rep(0, length(T))
Talta[T>=26 & T<31] <- (T[ T>=26 & T<31 ] - 26)/(31-26)
Talta[T>=31] <- 1
plot(T, Talta, col="green", xlab="x", ylab="y", main="Funciones pertenencia temperatura alta")



Tbaja.y.media <- pmin(Tbaja, Tmedia)
plot(T, Tbaja.y.media, col="green", xlab="x", ylab="y", main="Funciones pertenencia temperatura baja y media")

Tmedia.o.alta <- pmax(Tmedia, Talta)
plot(T, Tmedia.o.alta, col="green", xlab="x", ylab="y", main="Funciones pertenencia temperatura media o alta")

Tnomedia <- rep(0, length(T))
Tnomedia[T < 17] <- 1
Tnomedia[T >= 17 & T < 23] <- (23 - T[T >= 17 & T<23])/(23-17)
Tnomedia[T >= 23 & T < 27] <- (T[T >= 23 & T < 27] - 23)/(27-23)
Tnomedia[T >= 27] <- 1

Tnomedia.y.alta <- pmin(Tnomedia, Talta)
plot(T, Tnomedia.y.alta, col="green", xlab="x", ylab="y", main="Funciones pertenencia temperatura no media y alta")



funcion1.practica1 <- function( tipo, x, a, b, c)
{
  	## Esta funcion te pasa los parametros necesarios y tiene que devolver el la función de permanencia que crea conveniente
	##a, b y c son los tres parametros necesarios para crear las funciones de permanencia
	##el tipo puede ser triangular
  	##X todo el array
	
	if( tipo == "triangular" )
	{
		funcionPertenencia <- rep(0, length(X))
		funcionPertenencia[X >= a & X < b] <- (X[X >=a & X < b] - a)/(b - a)
		funcionPertenencia[X >= b & X < c] <- (c - X[X >=b & X < c])/(c - b)
	}
	else
	{
		if( a < b )
		{
			funcionPertenencia <- rep(0, length(X))
			funcionPertenencia[X < a] <- 1
			funcionPertenencia[X >= a & X < b] <- (b - X[X >=a & X < b]) / (b-a)
		}
		else
		{
			funcionPertenencia <- rep(0, length(X))
			funcionPertenencia[X > b & X <= a] <- (X[X > b & X <= a] - b) / (a - b)
			funcionPertenencia[X > a] <- 1
		}
	} 
	
	funcionPertenencia 
}

funcion2.practica1 <- function(tipo, funcionapertenencia1, funcionapertenencia2)
{
	##tipo puede ser unión, intersección, o complementación
	##si se elige complementación solo se pasa una función
	if( tipo == "union" )
	{
		funcionPertenenciaFinal <- pmax(funcionpertenencia1, funcionpertenencia2)
	}
	else
	{
		if( tipo = "interseccion" )
		{
			funcionPertenenciaFinal <- pmin(funcionpertenencia1, funcionpertenencia2)
		}
		else
		{
			if( tipo == "complementacion"
			{
				funcionPertenenciaFinal <- (1 - funcionpertenencia1)
			}
		}
	}
	funcionPertenenciaFinal
}