library(ggplot2)
library(GGally)
library(gridExtra)
library(palmerpenguins)

pengu <- penguins

distancia <- function(a,b){
  return(sqrt((a[1]-b[1])**2+(a[2]-b[2])**2))
}

kmedias <- function(matriz,k.clus){

  matriz <- matriz[!is.na(matriz[,1]) & !is.na(matriz[,2]),]
  
  centros <- matriz[sample(1:dim(matriz)[1],replace = FALSE,size = k.clus),]
  centros[,1] <- as.double(unlist(centros[,1]))
  centros[,2] <- as.double(unlist(centros[,2]))
  
  m <- cbind(matriz,NA)
  m <- cbind(m,NA)
  acum <- c()
  k <- 1
  print(centros)
  while (TRUE){
    
    #asigno cada punto a su centro
    
    for (i in (1:length(m[,1]))){
      for (j in 1:k.clus){
        if((is.na(m[i,3])) | (distancia(m[i,1:2], centros[j,]) < m[i,3])){
          m[i,3] <- distancia(m[i,1:2], centros[j,])
          m[i,4] <- j
        }
      }
    }
    acum[k] <- sum(m[,3])
    
    
    #recalculo centros
    
    for (l in 1:k.clus){
      centros[l,1] <- mean(m[m[,4] == l,1])
      centros[l,2] <- mean(m[m[,4] == l,2])
    }
    print(centros)
    
    
    if ((k > 1) && (acum[k] == acum[k-1])){
      break
    }
    k <- k+1
  }
  return(list(m[,4],centros,plot(acum,type = "l")))
}



# flipper vs bill length

mat <- pengu[,c("flipper_length_mm","bill_length_mm")]
set.seed(120)
kmedias(mat,3)
color <- kmedias(mat,3)[[1]]

plot(mat[!is.na(mat[,1]) & !is.na(mat[,2]),],col = color)

