# Simula_unif crea N vectores de dimensión dim
# con valores aleatorios (según una distribución uniforme) dentro de rango.

simula_unif <- function(N, dim, rango){
  res <- data.frame(matrix(nrow = N, ncol = dim))
  
  for(i in 1:N){
    res[i,] <- runif(dim, rango[1], rango[length(rango)])
  }
  
  return(res)
}

simula_gauss <- function(N, dim, sigma){
  res <- data.frame(matrix(nrow = N, ncol = dim))
  
  for(i in 1:N){
    res[i,] <- rnorm(dim, sd = sqrt(sigma))
  }
  
  return(res)
}

lista1 <- simula_unif(50, 2, -50:50)
lista2 <- simula_gauss(50, 2, 5)

print(lista1)
plot(lista1[,1], lista2[,2])
