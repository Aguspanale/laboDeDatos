library(ggplot2)
library(palmerpenguins)

datos <- matrix(nrow = 8,ncol = 2)

datos[1,] <- c(1,2)
datos[2,] <- c(2,1)
datos[3,] <- c(6,1)
datos[4,] <- c(7,2)
datos[5,] <- c(6,5)
datos[6,] <- c(6,7)
datos[7,] <- c(5,6)
datos[8,] <- c(1,1)

datosDF <- data.frame(datos)

ggplot(datosDF,aes(X1,X2)) + geom_point()

##para testear
muestras <- datosDF
k = 3

###





distanciaE <- function(a,b){
  return((a[1]-b[1])**2+(a[2]-b[2])**2)
}



kmedias <- function(muestras,k){
  ## limpio los datos pasando a double y sacando los NA
  
  muestras <- muestras[!is.na(muestras[,1]) & !is.na(muestras[,2]),]
  
  ## primero obtengo mis centros iniciales
  
  centros <- muestras[sample((1:dim(muestras)[1]),replace = FALSE, size = k),]
  centros[,1] <- as.double(unlist(centros[,1]))
  centros[,2] <- as.double(unlist(centros[,2]))
  
  ## armo una matriz para llenar con las distancias de todos los puntos a todos los centros
  
  distanciaACentros <- matrix(nrow = dim(muestras)[1],ncol = k)
  n <- 1
  error <- c()
  while(n < 100){
    
    for (i in 1:dim(muestras)[1]){
      for (j in 1:k){
        distanciaACentros[i,j] <- as.numeric(distanciaE(muestras[i,],centros[j,]))
       
      }
    }

   
    
    ##hago un vector que para cada elemento tiene el indice de su centro actual (que será el que tenga mas "cerca")
    
    centrosAsignados <- c()
    
    for (i in 1:dim(muestras)[1]){
      centrosAsignados[i] <- which.min(distanciaACentros[i,])
    }
    

    
    for (i in 1:k){
      centros[i,1] <- apply(muestras[centrosAsignados == i,],2,"mean")[1]
      centros[i,2] <- apply(muestras[centrosAsignados == i,],2,"mean")[2]
    }
    

    
    ## calculo el error como la suma de las distancias a los centros
    
    error[n] <- 0
    for (i in 1:k){
      error[n] <- error[n] + sum(distanciaACentros[centrosAsignados == i,i])
    }
    
    
    if (n > 1 && error[n] == error[n-1]){
      break
    }
    
    n <- n + 1
  }
  return(list(centrosAsignados, centros, error[n], plot(error,type = "l")))
}

kmedias(datosDF,3)[[4]]
ggplot(datosDF,aes(X1,X2)) + geom_point(color = kmedias(datosDF,3)[[1]])
  
pengu <- penguins

pengu <- pengu[,c("bill_length_mm", "flipper_length_mm")]

pengu <- pengu[!is.na(pengu[,1]) & !is.na(pengu[,2]),]

kmedias(pengu[,c("bill_length_mm", "flipper_length_mm")],3)

col <- kmedias(pengu[,c("bill_length_mm", "flipper_length_mm")],3)[[1]]

plot(pengu,col = col)

ggplot(pengu,aes(bill_length_mm,flipper_length_mm,color = kmedias(pengu[,c("bill_length_mm", "flipper_length_mm")],3)[[1]])) + geom_point()


pengu <- pengu[,c("bill_length_mm", "flipper_length_mm")]
