## ---- include = FALSE----------------------------------------------------
X11()
rm(list = ls())
par(mfrow=c(1,1))
set.seed(123456)

## ----uniforme------------------------------------------------------------
simula_unif <- function(N, dim, rango){
  res <- data.frame(matrix(nrow = N, ncol = dim))

  for(i in 1:N){
    res[i,] <- runif(dim, rango[1], rango[length(rango)])
  }
  
  names(res) <- c("X", "Y")

  return(res)
}

## ----gauss---------------------------------------------------------------
simula_gauss <- function(N, dim, sigma){
  res <- data.frame(matrix(nrow = N, ncol = dim))

  for(i in 1:N){
    res[i,] <- rnorm(dim, sd = sqrt(sigma))
  }
  
  names(res) <- c("X", "Y")

  return(res)
}

## ----datos---------------------------------------------------------------
muestra.uniforme <- simula_unif(50, 2, -50:50)
muestra.gaussiana <- simula_gauss(50, 2, 5:7)

## ----plotUniforme--------------------------------------------------------
plot(muestra.uniforme, main = "Distribución uniforme", xlab = "", ylab = "")
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----plotNormal----------------------------------------------------------
plot(muestra.gaussiana, main = "Distribución normal", xlab = "", ylab = "")
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----calculaRecta--------------------------------------------------------
simulaRecta <- function(intervalo){
  A <- runif(2, intervalo[1], intervalo[length(intervalo)])
  B <- runif(2, intervalo[1], intervalo[length(intervalo)])
  
  # La pendiente m es el cociente de las diferencias de las componentes
  m <- (A[2] - B[2]) / (A[1] - B[1])
  
  # La traslación b la sacamos despejando de y = ax + b
  b <- A[2] - m * A[1]
  
  # Lo devolvemos en este orden para ser consistentes con abline
  return(c(b,m))
}



## ----simulaRecta---------------------------------------------------------
rectaPrueba <- simulaRecta(-50:50)

plot(1, type="n", main="Recta de prueba", xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))
abline(coef = rectaPrueba)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----plotClasificadoRecta, tidy = TRUE-----------------------------------
rectaClasificacion <- simulaRecta(-50:50)

muestra.uniforme.etiquetada <- cbind(muestra.uniforme, Etiqueta = sign(muestra.uniforme$Y - rectaClasificacion[2]*muestra.uniforme$X - rectaClasificacion[1]))

muestra.uniforme.positiva <- subset(muestra.uniforme.etiquetada, Etiqueta ==  1)

muestra.uniforme.negativa <- subset(muestra.uniforme.etiquetada, Etiqueta == -1)

formas <- ifelse(muestra.uniforme.etiquetada$Etiqueta == 1, "+" , "-")
colores <- ifelse(muestra.uniforme.etiquetada$Etiqueta == 1, "red" , "blue")

plot(muestra.uniforme, 
     pch = formas,
     col = colores)

abline(coef = rectaClasificacion)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----plotClasificadoF1, echo = FALSE-------------------------------------

f <- function(x,y) {(x-10)^2 + (y-20)^2 - 400}

x <- seq(-50,50,length=1000)
y <- seq(-50,50,length=1000)
z <- outer(x,y,f)

contour(x,y,z,levels=0,drawlabels=FALSE, main=expression((x-10)^2 + (y-20)^2 - 400), xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))

points(subset(muestra.uniforme, f(X,Y) > 0 ), 
       pch = "+", col = "red")
points(subset(muestra.uniforme, f(X,Y) <= 0 ), 
       pch = "-", col = "blue")

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----plotClasificadoF2, echo = FALSE-------------------------------------

f <- function(x,y) {0.5*(x+10)^2 + (y-20)^2 - 400}

x <- seq(-50,50,length=1000)
y <- seq(-50,50,length=1000)
z <- outer(x,y,f)

contour(x,y,z,levels=0,drawlabels=FALSE, main=expression(0.5*(x+10)^2 + (y-20)^2 - 400), xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))

points(subset(muestra.uniforme, f(X,Y) > 0 ), 
       pch = "+", col = "red")
points(subset(muestra.uniforme, f(X,Y) <= 0 ), 
       pch = "-", col = "blue")

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----plotClasificadoF3, echo = FALSE-------------------------------------

f <- function(x,y) {0.5*(x-10)^2 - (y+20)^2 - 400}

x <- seq(-50,50,length=1000)
y <- seq(-50,50,length=1000)
z <- outer(x,y,f)

contour(x,y,z,levels=0,drawlabels=FALSE, main=expression(0.5*(x-10)^2 - (y+20)^2 - 400), xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))

points(subset(muestra.uniforme, f(X,Y) > 0 ), 
       pch = "+", col = "red")
points(subset(muestra.uniforme, f(X,Y) <= 0 ), 
       pch = "-", col = "blue")

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----plotClasificadoF4, echo = FALSE-------------------------------------

f <- function(x,y) {y - 20*x^2 -5*x + 3}

x <- seq(-50,50,length=1000)
y <- seq(-50,50,length=1000)
z <- outer(x,y,f)

contour(x,y,z,levels=0,drawlabels=FALSE, main=expression(y - 20*x^2 -5*x + 3), xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))

points(subset(muestra.uniforme, f(X,Y) > 0 ), 
       pch = "+", col = "red")
points(subset(muestra.uniforme, f(X,Y) <= 0 ), 
       pch = "-", col = "blue")

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----cambiaMuestra, tidy = TRUE------------------------------------------

# Tomamos la porción deseada
porcion <- 10
numero.datos.positivos <- ceiling(nrow(muestra.uniforme.positiva) * porcion / 100)
numero.datos.negativos <- ceiling(nrow(muestra.uniforme.negativa) * porcion / 100)

# Generamos vectores de booleanos
vector.aleatorio.muestras.positivas <- c(rep(FALSE,numero.datos.positivos), 
                                         rep(TRUE, nrow(muestra.uniforme.positiva) - numero.datos.positivos))

vector.aleatorio.muestras.negativas <- c(rep(FALSE,numero.datos.negativos), 
                                         rep(TRUE, nrow(muestra.uniforme.negativa) - numero.datos.negativos))

# Los reordenamos
vector.aleatorio.muestras.positivas <- sample(vector.aleatorio.muestras.positivas)
vector.aleatorio.muestras.negativas <- sample(vector.aleatorio.muestras.negativas)

# Separamos
muestra.uniforme.positiva.nueva <- 
  muestra.uniforme.positiva[vector.aleatorio.muestras.positivas,]
muestra.uniforme.negativa.nueva <- 
  muestra.uniforme.negativa[vector.aleatorio.muestras.negativas,]

# Volvemos a unir
muestra.uniforme.positiva.nueva <- rbind(muestra.uniforme.positiva.nueva,
                                       muestra.uniforme.negativa[!vector.aleatorio.muestras.negativas,])

muestra.uniforme.negativa.nueva <- rbind(muestra.uniforme.negativa.nueva,
                                         muestra.uniforme.positiva[!vector.aleatorio.muestras.positivas,])

muestra.uniforme.positiva.nueva$Etiqueta <-  1
muestra.uniforme.negativa.nueva$Etiqueta <- -1

## ----nuevaMuestraClasificada, echo = FALSE-------------------------------

plot(1, type="n", xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))

points(muestra.uniforme.positiva.nueva, pch = "+", col = "red")
points(muestra.uniforme.negativa.nueva, pch = "-", col = "blue")

abline(coef = rectaClasificacion)

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----funcionPLA----------------------------------------------------------
ajusta_PLA <- function(datos, label, max_iter, vini){
  cambio <- TRUE
  w <- vini
  iteraciones <- 0
  errores <- 0
  X <- cbind(1,datos)
  
  while (cambio && iteraciones < max_iter){
    cambio <- FALSE
    errores <- 0
    for (i in 1:nrow(datos)){
      x_i <- as.numeric(X[i,])
      prodEscalar <- crossprod(w,x_i)
      if (sign(prodEscalar) != label[i]){
        cambio <- TRUE
        errores <- errores + 1
        w <- w + label[i] * x_i
      }
    }
    iteraciones <- iteraciones + 1
  }

  resultado <- list("Peso inicial" = vini ,"Pesos" = w, 
                    "Iteraciones" = iteraciones, "Errores" = errores,
                    "Recta" = c(-w[1]/w[3], -w[2]/w[3]) )
  return (resultado)
}

## ----clasificacionPruebaPLA----------------------------------------------

etiquetas <- t(as.vector(muestra.uniforme.etiquetada["Etiqueta"]))
vini <- rep(0,3)

resultado <- ajusta_PLA(muestra.uniforme, etiquetas, 100, vini)

w <- as.vector(resultado$Pesos)
iteraciones <- as.numeric(resultado$Iteraciones)

cat("Iteraciones PLA con vector inicial (0,0,0)")
print(iteraciones)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

mediaIteraciones <- 0

for (i in 1:10){
  vini <- runif(3, 0,1)
  cat("Pesos:")
  print(vini)
  resultado <- ajusta_PLA(muestra.uniforme, etiquetas, 100, vini) 
  mediaIteraciones <- mediaIteraciones + as.numeric(resultado$Iteraciones)
  cat("Iteraciones")
  print(resultado$Iteraciones)
}
mediaIteraciones <- mediaIteraciones / 10
cat("Media de iteraciones:")
print(mediaIteraciones)

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----clasificacionNoSeparable--------------------------------------------
muestra.uniforme.no.separable <- rbind(cbind(muestra.uniforme.positiva.nueva),
                                       cbind(muestra.uniforme.negativa.nueva))

rownames(muestra.uniforme.no.separable) <- NULL

etiquetas <- t(as.vector(muestra.uniforme.no.separable["Etiqueta"]))

vini <- rep(0,3)

resultado <- ajusta_PLA(muestra.uniforme.no.separable[,c("X","Y")], etiquetas, 10, vini)
cat("PLA con límite 10")
print(resultado$Errores)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

resultado <- ajusta_PLA(muestra.uniforme.no.separable[,c("X","Y")], etiquetas, 100, vini)
cat("PLA con límite 100")
print(resultado$Errores)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

resultado <- ajusta_PLA(muestra.uniforme.no.separable[,c("X","Y")], etiquetas, 1000, vini)
cat("PLA con límite 1000")
print(resultado$Errores)

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----clasificacionCircular-----------------------------------------------
f <- function(x,y) {(x-10)^2 + (y-20)^2 - 400}

muestra.uniforme.circular <- cbind(muestra.uniforme, 
                                   Etiqueta = sign(f(muestra.uniforme$X, muestra.uniforme$Y)))

etiquetas <- t(as.vector(muestra.uniforme.circular["Etiqueta"]))

vini <- rep(0,3)

resultado <- ajusta_PLA(muestra.uniforme.circular[,c("X","Y")], etiquetas, 10, vini)
cat("PLA con límite 10")
print(resultado$Errores)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

resultado <- ajusta_PLA(muestra.uniforme.circular[,c("X","Y")], etiquetas, 100, vini)
cat("PLA con límite 100")
print(resultado$Errores)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

resultado <- ajusta_PLA(muestra.uniforme.circular[,c("X","Y")], etiquetas, 1000, vini)
cat("PLA con límite 1000")
print(resultado$Errores)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

plot(1, type="n", xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))

points(subset(muestra.uniforme.circular,Etiqueta ==  1), pch = "+", col = "red")
points(subset(muestra.uniforme.circular,Etiqueta == -1), pch = "-", col = "blue")

abline(coef = resultado$Recta)

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----ajusta_PLA_Plot-----------------------------------------------------

ajusta_PLA_PLOT <- function(datos, label, max_iter, vini){
  cambio <- TRUE
  w <- vini
  iteraciones <- 0
  errores <- 0
  X <- cbind(1,datos)
  
  datos.etiquetados <- cbind(datos, Etiqueta = t(label))

  while (cambio && iteraciones < max_iter){
    cambio <- FALSE
    errores <- 0
    for (i in 1:nrow(datos)){
      x_i <- as.numeric(X[i,])
      prodEscalar <- crossprod(w,x_i)
      if (sign(prodEscalar) != label[i]){
        cambio <- TRUE
        errores <- errores + 1
        w <- w + label[i] * x_i
      }
    }
    ### PLOT
    recta <- c(-w[1]/w[3], -w[2]/w[3])
    plot(0, type="n", xlab="", ylab="", xlim=c(-50, 50), ylim=c(-50, 50))

    points(subset(datos.etiquetados,Etiqueta ==  1), pch = "+", col = "red")
    points(subset(datos.etiquetados,Etiqueta == -1), pch = "-", col = "blue")
    
    abline(coef = recta)
    
    ##############
    iteraciones <- iteraciones + 1
    print(paste("Iteracion", iteraciones))
    print(paste("Errores:",errores))
    
    cat("Pulsa Enter para seguir")
    invisible(readLines("stdin", n=1))
  }

  resultado <- list("Peso inicial" = vini ,"Pesos" = w, 
                    "Iteraciones" = iteraciones, "Errores" = errores,
                    "Recta" = c(-w[1]/w[3], -w[2]/w[3]) )
  return (resultado)
}

vini <- rep(0,3)

resultado <- ajusta_PLA_PLOT(muestra.uniforme.circular[,c("X","Y")], etiquetas, 15, vini)
cat("Errores totales:")
print(resultado$Errores)

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
## ----funcionPLAMOD-------------------------------------------------------
ajusta_PLA_MOD <- function(datos, label, max_iter, vini){
  cambio <- TRUE
  w <- vini
  iteraciones <- 0
  errores <- 0
  X <- cbind(1,datos)
  mejor <- vini
  mejor_errores <- nrow(datos)
  
  while (cambio && iteraciones < max_iter){
    cambio <- FALSE
    errores <- 0
    for (i in 1:nrow(datos)){
      x_i <- as.numeric(X[i,])
      prodEscalar <- crossprod(w,x_i)
      if (sign(prodEscalar) != label[i]){
        cambio <- TRUE
        errores <- errores + 1
        w <- w + label[i] * x_i
      }
    }
    if (errores < mejor_errores){
      mejor <- w
      mejor_errores <- errores
    }
    iteraciones <- iteraciones + 1
  }

  resultado <- list("Peso inicial" = vini ,"Pesos" = mejor, 
                    "Iteraciones" = iteraciones, "Errores" = mejor_errores,
                    "Recta" = c(-w[1]/w[3], -w[2]/w[3]) )
  return (resultado)
}

## ----pruebaMOD-----------------------------------------------------------
resultado <- ajusta_PLA_MOD(muestra.uniforme.circular[,c("X","Y")], etiquetas, 15, vini)
cat("Errores totales:")
print(resultado$Errores)

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))


## ----echo = FALSE, include = FALSE---------------------------------------
# Borrar variables que NO son funciones
rm(list = setdiff(ls(), lsf.str()))

## ----leerZip-------------------------------------------------------------
archivo  <- read.csv("datos/zip.train", header = FALSE, sep = " ")
colnames(archivo)[1] <- "ID"
archivo["V258"] <- NULL

numeros_5 <- subset(archivo, ID == 5)
numeros_5["ID"] <- NULL
numeros_5 <- data.matrix(numeros_5)

numeros_1 <- subset(archivo, ID == 1)
numeros_1["ID"] <- NULL
numeros_1 <- data.matrix(numeros_1)

numeros_5_matrix <- lapply(split(numeros_5, 1:nrow(numeros_5)), 
                           function(m) { 
                             mtx <- matrix(0.5*(1-m), nrow = 16, ncol = 16)
                             return(mtx[,16:1])
                             })
image(numeros_5_matrix[[1]], col = gray.colors(256))

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

numeros_1_matrix <- lapply(split(numeros_1, 1:nrow(numeros_1)), 
                           function(m) { 
                             mtx <- matrix(0.5*(1-m), nrow = 16, ncol = 16)
                             return(mtx[,16:1])
                             })
image(numeros_1_matrix[[50]], col = gray.colors(256))

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----mediaSimetriaVertical-----------------------------------------------
medias_1 <- lapply(numeros_1_matrix, mean)
medias_5 <- lapply(numeros_5_matrix, mean)

simetriaVertical <- function(vector) {
  simetria <- 2*sum(abs(vector[1:(length(vector)/2)] - vector[(length(vector)/2+1):length(vector)]))
  return(simetria)
}

simetrias_1 <- lapply(numeros_1_matrix,function(m) -sum(apply(m,1,simetriaVertical)) )
simetrias_5 <- lapply(numeros_5_matrix,function(m) -sum(apply(m,1,simetriaVertical)) )

## ----plotMediaSimetria---------------------------------------------------
datos_1 <- data.frame(Medias = as.numeric(medias_1), Simetrias = as.numeric(simetrias_1), 
                      Etiqueta = 1)
datos_5 <- data.frame(Medias = as.numeric(medias_5), Simetrias = as.numeric(simetrias_5), 
                      Etiqueta = -1)

datos <- rbind(datos_1, datos_5)
plot(datos$Medias, datos$Simetrias, main = "Intensidad y Simetría", 
     xlab = "Intensidad", ylab = "Simetría Vertical", col = as.numeric(datos$Etiqueta) + 2)

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----regresion-----------------------------------------------------------
regres_lin <- function(datos, label, pesos = FALSE) {
  
  # Si no hay columnas es porque le hemos pasado un vector para hacer regresión
  # Si hay columnas es porque queremos hacer un clasificador
  if (is.null(ncol(datos)))
    X <- cbind(1, as.numeric(datos))
  else
    X <- cbind(1, data.matrix(datos))
  
  X.svd <- svd(X)

  V <- X.svd$v
  D <- X.svd$d
  Di <- diag(ifelse(D>0.0001, 1/D, D))
  
  Xt.X.inv <- V %*% Di^2 %*% t(V)
  
  pseudoinversa <- Xt.X.inv %*% t(X)
  
  w <- pseudoinversa %*% as.numeric(label)
  
  # Si es regresión o el vector de pesos es distinto a 3, se devuelve el vector
  # Si no, se devuelve la recta asociada
  if (is.null(ncol(datos)) || length(w) != 3 || pesos == TRUE)
    return(w)
  else
    return(c(-w[1]/w[3], -w[2]/w[3]))
}

## ----pruebaRegresion-----------------------------------------------------
muestras.clasificacion <- cbind(datos$Medias, datos$Simetrias)
etiquetas.clasificacion <- datos$Etiqueta
muestras.regresion <- datos$Medias
etiquetas.regresion <- datos$Simetrias
recta.clasificacion <- regres_lin(muestras.clasificacion, etiquetas.clasificacion)
recta.regresion <- regres_lin(muestras.regresion, etiquetas.regresion)

plot(datos$Medias, datos$Simetrias, main = "Intensidad y Simetría", xlab = "Intensidad", ylab = "Simetría Vertical", col = as.numeric(datos$Etiqueta) + 2)

abline(recta.clasificacion, col = "red")
abline(recta.regresion, col = "blue")

cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----Ein-----------------------------------------------------------------
intervalo <- -10:10
muestras <- simula_unif(100, 2, intervalo)

errores <- 0
for (i in 1:1000) {
  f <- simulaRecta(intervalo)
  etiquetas <- sign(muestras$Y - f[2]*muestras$X - f[1])
  g <- regres_lin(muestras,etiquetas)
  etiquetas.regresion <- sign(muestras$Y - g[2]*muestras$X - g[1])
  
  errores <- errores + sum(etiquetas != etiquetas.regresion)/length(etiquetas)
}
errores <- errores / 1000

cat("Error dentro de la muestra (en %): ")
print(errores*100)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
cat("Este tarda bastante...")

## ----Eout, eval = TRUE---------------------------------------------------
intervalo <- -10:10
muestras <- simula_unif(100, 2, intervalo)

errores <- 0
for (i in 1:1000) {
  f <- simulaRecta(intervalo)
  etiquetas <- sign(muestras$Y - f[2]*muestras$X - f[1])
  g <- regres_lin(muestras,etiquetas)
  
  muestras.nuevas <- simula_unif(1000, 2, intervalo)
  etiquetas.f <- sign(muestras.nuevas$Y - f[2]*muestras.nuevas$X - f[1])
  etiquetas.g <- sign(muestras.nuevas$Y - g[2]*muestras.nuevas$X - g[1])
  
  errores <- errores + sum(etiquetas.f != etiquetas.g)/length(etiquetas.f)
}
errores <- errores / 1000

cat("Error fuera de la muestra (en %): ")
print(errores*100)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----RegresionPLA--------------------------------------------------------
intervalo <- -10:10
muestras <- simula_unif(10, 2, intervalo)

iteraciones <- 0
iteraciones.cero <- 0
for (i in 1:100) {
  f <- simulaRecta(intervalo)
  etiquetas <- sign(muestras$Y - f[2]*muestras$X - f[1])
  w <- regres_lin(muestras,etiquetas, TRUE)
  w.cero <- rep(0,3)
  
  resultado <- ajusta_PLA(muestras, etiquetas, 10000, w)
  resultado.cero <- ajusta_PLA(muestras, etiquetas, 10000, w.cero)
  
  iteraciones <- iteraciones + resultado$Iteraciones
  iteraciones.cero <- iteraciones.cero + resultado.cero$Iteraciones
}
iteraciones <- iteraciones / 100
iteraciones.cero <- iteraciones.cero / 100

cat("Iteraciones con vector inicial el de regresión: ")
print(iteraciones)
cat("Iteraciones con vector inicial el (0,0,0): ")
print(iteraciones.cero)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))


## ----echo = FALSE, include = FALSE---------------------------------------
# Borrar variables que NO son funciones
rm(list = setdiff(ls(), lsf.str()))

## ----noLinealMuestreo----------------------------------------------------
f <- function(X,Y) { sign(X^2 + Y^2 - 0.6) }
muestras <- simula_unif(1000, 2, c(-10,10))

etiquetas <- apply(muestras,1, function(m) { do.call(f, as.list(m)) } )

porcion <- 10
numero.datos.ruido <- ceiling(nrow(muestras) * porcion / 100)
Ein <- 0

for (i in 1:1000) {
  ruido <- sample(c(rep(-1,numero.datos.ruido), 
                                           rep(1, nrow(muestras) - numero.datos.ruido)))
  etiquetas.ruido <- etiquetas * ruido
  
  g <- regres_lin(muestras,etiquetas)
  etiquetas.g <- sign(muestras$Y - g[2]*muestras$X - g[1])
  
  Ein <- Ein + sum(etiquetas.ruido != etiquetas.g)/length(etiquetas.ruido)
}

Ein <- Ein / 1000

cat("Error dentro de la muestra (en %): ")
print(Ein*100)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))

## ----RegresionTransformada-----------------------------------------------
muestras.transformadas <- data.frame(X0 = 1, X1 = muestras$X, X2 = muestras$Y,
                                     X1X2 = muestras$X * muestras$Y, 
                                     X1_2 = (muestras$X)^2,
                                     X2_2 = (muestras$Y)^2 )

regresion.transformada <- regres_lin(muestras.transformadas[,-1], etiquetas.ruido)

print("Pesos regresión con transformación: ")
print(regresion.transformada)
cat("Pulsa Enter para seguir")
invisible(readLines("stdin", n=1))
cat("Este tarda bastante...")


## ----EoutRegresionTransformada, eval = TRUE------------------------------
Eout <- 0

for (i in 1:1000) {
  muestras.fuera <- simula_unif(1000, 2, c(-10,10))
  muestras.transformadas.fuera <- data.frame(X0 = 1, X1 = muestras.fuera$X, 
                                     X2 = muestras.fuera$Y,
                                     X1X2 = muestras.fuera$X * muestras.fuera$Y, 
                                     X1_2 = (muestras.fuera$X)^2,
                                     X2_2 = (muestras.fuera$Y)^2 )
  
  
  etiquetas.buenas <- apply(muestras.fuera, 1, function(m) { do.call(f, as.list(m)) } )
  pesos.transformada <- regres_lin(muestras.transformadas[,-1], etiquetas.ruido)
  etiquetas.fuera <- apply(muestras.transformadas.fuera, 1, function(m) { 
                                                              sign(sum(pesos.transformada * m)) 
                                                            } )
  
  Eout <- Eout + sum(etiquetas.buenas != etiquetas.fuera)/length(etiquetas.buenas)
  
}

Eout <- Eout / 1000

cat("Error fuera de la muestra (en %): ")
print(Eout*100)
