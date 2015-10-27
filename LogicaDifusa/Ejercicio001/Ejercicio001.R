# Cargamos la librería 
library( MASS )

# Generamos la distribución gaussiana
dist.gauss.mult <- mvrnorm( n=300, mu=c(0,0), Sigma=matrix( c(0.1, 0, 0, 0.1), nrow=2))

# Generamos la distribución uniforme
unif.cuadrada <- matrix( data = runif(600, -2, -1), nrow=300, 2 )


## concatenamos por filas
matriz.datos.gauss.cuadrada <- rbind( dist.gauss.mult, unif.cuadrada)  

# vector de etiquetas
etiqueta <- rep( c(1, 2), c(300, 300) )

#dibujamos
plot( matriz.datos.gauss.cuadrada, col=etiqueta, main="")

