# Ejercicios: Distribucion Exponencial(0.25)

library(ggplot2)

#-------------------------------------------------
# Ejercicio 1: Realizar un grÃ¡fico de n vs mean(x_n)
promedios_muestrales_1a <- numeric(3000)
promedios_muestrales_1b <- numeric(3000)

# CASO SEED ADENTRO DEL FOR
for (i in 1:3000){
  set.seed(0)
  muestraExp <- rexp(i, 0.25)                     #genero una muestra de tamaÃ±o i
  promedios_muestrales_1a[i] <- mean(muestraExp)  #calculo la media muestral
}

# CASO SEED AFUERA DEL FOR
set.seed(0)
for (i in 1:3000){
  muestraExp <- rexp(i, 0.25)
  promedios_muestrales_1b[i] <- mean(muestraExp)
}

plot(promedios_muestrales_1a)
plot(promedios_muestrales_1b)

#-------------------------------------------------

# Ejercicio 2:

promedio_muestral <- function(n){
  promedios_muestrales <- numeric(1000)
  
  for(i in 1:1000){
    muestra <- rexp(n, 0.25)
    promedios_muestrales[i] <- mean(muestra)
  }
  
  return(promedios_muestrales)
}

#a) caso n = 2
promedios_muestrales_2 <- promedio_muestral(2)
hist(promedios_muestrales_2, , prob=TRUE)
boxplot(promedios_muestrales_2)
qqnorm(promedios_muestrales_2)
qqline(promedios_muestrales_2)

#b) caso n = 5
promedios_muestrales_5 <- promedio_muestral(5)
hist(promedios_muestrales_5)
boxplot(promedios_muestrales_5)
qqnorm(promedios_muestrales_5)
qqline(promedios_muestrales_5)

#c.1) caso n = 30
promedios_muestrales_30 <- promedio_muestral(30)
hist(promedios_muestrales_30)
boxplot(promedios_muestrales_30)
qqnorm(promedios_muestrales_30)
qqline(promedios_muestrales_30)

#c.2) caso n = 500
promedios_muestrales_500 <- promedio_muestral(500)
hist(promedios_muestrales_500)
boxplot(promedios_muestrales_500)
qqnorm(promedios_muestrales_500)
qqline(promedios_muestrales_500)

#d) aclarar que pasa si se siguiera aumentando el tamaÃ±o de la muestra

#e) boxplot comparativo
boxplot(promedios_muestrales_2,promedios_muestrales_5,promedios_muestrales_30,promedios_muestrales_500)

#-------------------------------------------------

#Ejercicio 3:

normalizacion <- function(promedios_muestrales){
  esperanza_teorica <- 4
  varianza_teorica <- 16
  promedios_normalizados <- numeric(length(promedios_muestrales))
  
  for (i in 0:length(promedios_muestrales)){
    promedios_normalizados[i] <- (promedios_muestrales[i] - esperanza_teorica)/(sqrt(varianza_teorica/length(promedios_muestrales)))
  }
  
  return(promedios_normalizados)
}

#b) Realizar la transformacion mencionada en los 4 conjuntos de datos del ej2. 
n2 = normalizacion(promedios_muestrales_2)
n5 = normalizacion(promedios_muestrales_5)
n30 = normalizacion(promedios_muestrales_30)
n500 = normalizacion(promedios_muestrales_500)

#Graficar boxplots paralelos y QQ-plots.
qqnorm(n2)
qqline(n2)

qqnorm(n5)
qqline(n5)

qqnorm(n30)
qqline(n30)

qqnorm(n500)
qqline(n500)

boxplot(n2,n5,n30,n500)

#c) Realizar 4 histogramas y a cada uno de ellos superponerle la densidad de la normal estandar.
#OBS: Estan mal los parametros de la normal, tendrÃ­an que ser N(0,1) creo, pero quedaba feo asi que por las dudas lo dejo asi por ahora.

hist(n2, prob=TRUE)
curve(dnorm(x, mean=mean(n2), sd=sd(n2)), add=TRUE)

hist(n5, prob=TRUE)
curve(dnorm(x, mean=mean(n5), sd=sd(n5)), add=TRUE)

hist(n30, prob=TRUE)
curve(dnorm(x, mean=mean(n30), sd=sd(n30)), add=TRUE)

hist(n500, prob=TRUE)
curve(dnorm(x, mean=mean(n500), sd=sd(n500)), add=TRUE)

