library(ggplot2)
library(GGally)
library(gridExtra)

#      v <- c(2,3.,3,4,3.5,3,8,5,9,4)
#      
#      m <- matrix(v,5,2,TRUE)
#      
#      k <- 2
#      
#      centros <- m[sample(1:length(m[,1]),replace = FALSE,size = k),]
#      
#      m <- cbind(m,NA)
#      m <- cbind(m,NA)
#      
#      
#      #asigno cada punto a su centro
#      
#      for (i in (1:length(m[,1]))){
#        m[i,3] <- norm((m[i,1:2]- centros[m[i,4],]),type = "2")
#        for (j in (1:k)){
#          if((is.na(m[i,3])) | (norm((m[i,1:2] - centros[j,]),type = "2") < m[i,3])){
#            m[i,3] <- norm((m[i,1:2]- centros[j,]),type = "2")
#            m[i,4] <- j
#          }
#        }
#      }
#      
#      #recalculo centros
#      
#      for (i in 1:k){
#        centros[i,1] <- mean(m[m[,4] == i,1])
#        centros[i,2] <- mean(m[m[,4] == i,2])
#      }



kmedias <- function(mat,k.clus,n.iter){

  
  centros <- mat[sample(1:length(mat[,1]),replace = FALSE,size = k.clus),]
  
  m <- cbind(mat,NA)
  m <- cbind(m,NA)
  
  acum <- c()
  
  for (k in 1:n.iter){
    
    #asigno cada punto a su centro
    
    for (i in (1:length(m[,1]))){
      m[i,3] <- norm((m[i,1:2]- centros[m[i,4],]),type = "2")
      for (j in (1:k.clus)){
        if((is.na(m[i,3])) | (norm((m[i,1:2] - centros[j,]),type = "2") < m[i,3])){
          m[i,3] <- norm((m[i,1:2]- centros[j,]),type = "2")
          m[i,4] <- j
        }
      }
    }
    acum[k] <- sum(m[,3])
    
    #recalculo centros
    
    for (i in 1:k.clus){
      centros[i,1] <- mean(m[m[,4] == i,1])
      centros[i,2] <- mean(m[m[,4] == i,2])
    }
    
  }
  return(list(m[,4],centros,lines(acum)))
}

m <- matrix(v,5,2,TRUE)
kmedias(m,2,4)
