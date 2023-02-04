#Ejercicio 1

a <- c(-1,0,1)

d <- (a[2]**2-4*a[3]*a[1])

r1 <- (-a[1]+sqrt(d))/2*a[3]

r2 <- (-a[1]-sqrt(d))/2*a[3]

#Ejercicio 2

raices <- function(a){
  d <- (a[2]**2-4*a[3]*a[1])
  if (d < 0) {
    print("El polinomio no tiene raices reales")
    return()
  }
  r1 <- (-a[1]+sqrt(d))/2*a[3]
  
  r2 <- (-a[1]-sqrt(d))/2*a[3]
  return(c(r1,r2))
}

raices(a)

#Ejercicio 3

#A

letra <- function(){
  return(sample(letters, 1))
}
letra()

#B

help(paste)
paste("hola","a",sep="")

crearPalabra <- function(palabra){
  letra <- letra()
  palabra <- paste(palabra,letra,sep = "")
  while(letra != "a"){
    letra <- letra()
    palabra <- paste(palabra,letra,sep = "")
  }
  return(palabra)
}
crearPalabra("")

#C

promedioDeLetras <- function(n){
  letras <- 0
  for(i in 1:3){
    letras <- letras + nchar(crearPalabra(""))
  }
  return(letras/n)
}
promedioDeLetras(5)

#4

runif(1)
acumulador <- 0
cantidad <- 0
while(acumulador < 500){
  acumulador <- acumulador + runif(1)
  cantidad <- cantidad + 1
}
print(c(acumulador,cantidad))

#5

cuadrado <- function(lista){
  for(i in 1:length(lista)){
    if(lista[i]>0){
      lista[i] <- lista[i]*lista[i]
    }
  }
  return(lista)
}
cuadrado(c(-1,-1,4,3))

cuadrado2 <- function(lista){
  lista[lista>0] <- lista[lista>0]*lista[lista>0]
  return(lista)
}
cuadrado2(c(-1,-1,4,3))

#6 

A = matrix(c(1,3,2,8),2,2,TRUE)
solve(A,c(0,0))

#7

armarMat <- function(a,n){
  linea <- rep(0,n+1)
  linea[1] <- a
  return(matrix(linea,n,n))
}
armarMat(3,4)

#8

armarDiagonal <- function(v){
  matriz <- matrix(0,length(v),length(v))
  for(i in 1:length(v)){
    matriz[i,i] <- v[i]
  }
  return(matriz)
}
armarDiagonal(1:6)

#9
#a

astro <- read.csv("astronauts.csv")

astro$Gender <- factor(astro$Gender)
astro$Status <- factor(astro$Status)
astro$Military.Rank <- factor(astro$Military.Rank)

#b

table(astro$Space.Flights)

#c
#min
astro$Name[which.min(astro$Space.Flights)]
#max
astro$Name[which.max(astro$Space.Flights)]

#d

plot(astro$Space.Flights, astro$Space.Walks,type = "p", col = 4)
smoothScatter(astro$Space.Flights, astro$Space.Walks,col = 1)
points(astro$Space.Flights, astro$Space.Walks,col = 1)

#e
smoothScatter(astro$Space.Flight..hr, astro$Space.Walks..hr., col = 4)
       

#f

str(astro)
columnasNumericas <- sapply(astro, is.numeric)
astroNumericas <- astro[,columnasNumericas]
                            
suma <- apply(astroNumericas, 2, sum,na.rm = TRUE)

promedio <- apply(astroNumericas, 2, mean,na.rm = TRUE)

maximo <- apply(astroNumericas, 2, max,na.rm = TRUE)

minimo <- apply(astroNumericas, 2, min,na.rm = TRUE)

resumenDatos <- data.frame(Suma = suma, Promedio = promedio, Maximo = maximo, Minimo = minimo)

resumenDatos <- t(resumenDatos)
