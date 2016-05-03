## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(numDeriv)
library(ggplot2)
set.seed(12345678)
X11()

## ----funcionesAuxiliares, include=FALSE----------------------------------
simula_unif = function (N=2,dims=2, rango = c(0,1)){
 m = data.frame(matrix(runif(N*dims, min=rango[1], max=rango[2]),
 nrow = N, ncol=dims, byrow=T))
 
 colnames(m) <- c("X","Y")
 return(m)
}

simula_recta <- function(intervalo){
  A <- runif(2, intervalo[1], intervalo[length(intervalo)])
  B <- runif(2, intervalo[1], intervalo[length(intervalo)])
  
  # La pendiente m es el cociente de las diferencias de las componentes
  m <- (A[2] - B[2]) / (A[1] - B[1])
  
  # La traslación b la sacamos despejando de y = ax + b
  b <- A[2] - m * A[1]
  
  # Lo devolvemos en este orden para ser consistentes con abline
  return(c(b,m))
}

distance <- function(x,y) {
  sqrt(sum((x-y)^2))
}

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

ajusta_PLA_MOD <- function(datos, label, max_iter, vini){
  cambio <- TRUE
  w <- vini
  iteraciones <- 0
  errores <- 0
  X <- cbind(1,datos)
  mejor <- vini
  errores_inicial <- sum(apply(X, 1, function(x) { sign(crossprod(w,as.numeric(x))) }) != label)
  mejor_errores <- errores_inicial
  
  while (cambio && iteraciones < max_iter){
    cambio <- FALSE
    errores <- 0
    for (i in 1:nrow(datos)){
      x_i <- as.numeric(X[i,])
      prodEscalar <- crossprod(w,x_i)
      if (sign(prodEscalar) != label[i]){
        cambio <- TRUE
        w <- w + label[i] * x_i
      }
    }
    # for (i in 1:nrow(datos)){
    #   x_i <- as.numeric(X[i,])
    #   prodEscalar <- crossprod(w,x_i)
    #   if (sign(prodEscalar) != label[i]){
    #     errores <- errores + 1
    #   }
    # }
    errores <- sum(apply(X, 1, function(x) { sign(crossprod(w,as.numeric(x))) }) != label)
    if (errores < mejor_errores){
      mejor <- w
      mejor_errores <- errores
    }
    iteraciones <- iteraciones + 1
  }

  resultado <- list("Peso inicial" = vini ,"Pesos" = mejor, 
                    "Iteraciones" = iteraciones, "Errores" = mejor_errores,
                    "Error inicial" = errores_inicial,
                    "Recta" = c(-mejor[1]/mejor[3], -mejor[2]/mejor[3]) )
  return (resultado)
}

pausa <- function(){
  cat("Pulsa cualquier caracter")
  ch <- scan()
}

## ----gradienteDescendente------------------------------------------------
gradienteDescendente <- function(funcion, sol_inicial = rep(0,2), tol = -1e+14, 
                                 max_iter = 150000, tasa_aprendizaje = 0.1) {
  w <- sol_inicial
  t <- 1
  imagenes <- funcion(w)
  
  while (t < max_iter && funcion(w) > tol){
    gradiente <- grad(funcion, x=w)
    direccion <- -gradiente
    w <- w + tasa_aprendizaje * direccion
    imagenes <- c(imagenes, funcion(w))
    t <- t+1
  }
  return(list("Solucion" = w, "Valor" = funcion(w), "Iteraciones" = t, "Imagenes" = imagenes))
}

## ----1a------------------------------------------------------------------
  e <- function(w) {(w[1]*exp(w[2]) - 2*w[2]*exp(-w[1]))^2}
  resultado <- gradienteDescendente(e, c(1,1), tol = 1e-14)
  resultado[c("Solucion", "Valor", "Iteraciones")]
  print("Gradiente descendente")

## ----1b1, echo=FALSE-----------------------------------------------------
  f <- function(w) { w[1]^2 + 2*w[2]^2 + 2*sin(2*pi*w[1])*sin(2*pi*w[2]) }
  resultado <- gradienteDescendente(f, sol_inicial = c(1,1), max_iter = 50, tasa_aprendizaje = 0.01)

  resultado2 <- gradienteDescendente(f, sol_inicial = c(1,1), max_iter = 50, tasa_aprendizaje = 0.1)
  
  l1 <- length(resultado$Imagenes)
  l2 <- length(resultado2$Imagenes)
  
  datos <- data.frame(Iteraciones = 1:resultado$Iteraciones,
         Imagen1 = c(resultado$Imagenes,rep(resultado$Imagenes[l1-1], max(50 - l1,0)) ),
         Imagen2 = c(resultado2$Imagenes,rep(resultado2$Imagenes[l2-1], max(50 - l2,0)) )
  )
  
  ggplot(data=datos, aes(x = Iteraciones)) +
    geom_line(aes(y=Imagen1), colour = "blue") + 
    geom_line(aes(y=Imagen2), colour = "green") +
    labs(list(title = "Evolución de imágenes", x = "Iteraciones", y = "Imagen")) +
    scale_colour_manual(name = "Legend", values = c(1,2,3,4))
    
  pausa()


## ----1b2, echo=FALSE-----------------------------------------------------
  resultado <- gradienteDescendente(f, sol_inicial = c(0.1,0.1), max_iter = 50, tasa_aprendizaje = 0.01)
  resultado2 <- gradienteDescendente(f, sol_inicial = c(1,1), max_iter = 50, tasa_aprendizaje = 0.01)
  resultado3 <- gradienteDescendente(f, sol_inicial = c(-0.5,-0.5), max_iter = 50, tasa_aprendizaje = 0.01)
  resultado4 <- gradienteDescendente(f, sol_inicial = c(-1,-1), max_iter = 50, tasa_aprendizaje = 0.01)

  l1 <- length(resultado$Imagenes)
  l2 <- length(resultado2$Imagenes)
  l3 <- length(resultado3$Imagenes)
  l4 <- length(resultado4$Imagenes)
  
  datos <- data.frame(Iteraciones = 1:50,
         Imagen1 = c(resultado$Imagenes, rep(resultado$Imagenes[l1-1],  max(50 - l1,0)) ),
         Imagen2 = c(resultado2$Imagenes,rep(resultado2$Imagenes[l2-1], max(50 - l2,0)) ),
         Imagen3 = c(resultado3$Imagenes,rep(resultado3$Imagenes[l3-1], max(50 - l3,0)) ),
         Imagen4 = c(resultado4$Imagenes,rep(resultado4$Imagenes[l4-1], max(50 - l4,0)) )
  )
  
  ggplot(data=datos, aes(x = Iteraciones)) +
    geom_line(aes(y=Imagen1), colour = "blue") + 
    geom_line(aes(y=Imagen2), colour = "green") +
    geom_line(aes(y=Imagen3), colour = "red") + 
    geom_line(aes(y=Imagen4), colour = "black") +
    labs(list(title = "Evolución de imágenes", x = "Iteraciones", y = "Imagen")) 
  


## ----1b2.2, echo=FALSE---------------------------------------------------
  print("Gradiente descendente con distintos puntos de inicio")
  kable(datos)
  pausa()

## ----2-------------------------------------------------------------------
coordenadaDescendente <- function(funcion, sol_inicial = rep(0,2), tol = -1e+14, 
                                 max_iter = 150000, tasa_aprendizaje = 0.1) {
  w <- sol_inicial
  t <- 1
  imagenes <- funcion(w)
  
  while (t < max_iter && funcion(w) > tol){
    gradiente <- grad(funcion, x=w)
    direccion <- -gradiente
    w[1] <- w[1] + tasa_aprendizaje * direccion[1]
    
    gradiente <- grad(funcion, x=w)
    direccion <- -gradiente
    w[2] <- w[2] + tasa_aprendizaje * direccion[2]
    
    imagenes <- c(imagenes, funcion(w))
    t <- t+1
  }
  return(list("Solucion" = w, "Valor" = funcion(w), "Iteraciones" = t, "Imagenes" = imagenes))
}

## ----2a------------------------------------------------------------------
  resultado <- coordenadaDescendente(e, sol_inicial = c(1,1), max_iter = 15, tasa_aprendizaje = 0.1)
  resultado[c("Solucion", "Iteraciones", "Valor")]
  print("Coordenada descendente")
  pausa()
## ----Newton--------------------------------------------------------------
metodoNewton <- function(funcion, sol_inicial = rep(0,2), tol = -1e+14,
                         max_iter = 150000, tasa_aprendizaje = 0.1) {
  w <- sol_inicial
  t <- 1
  imagenes <- funcion(w)
  
  while (t < max_iter && funcion(w) > tol){
    H <- hessian(funcion, x=w)
    gradiente <- grad(funcion, x=w)
    direccion <- -solve(H)%*%gradiente
    
    w <- w + tasa_aprendizaje*direccion
    imagenes <- c(imagenes, funcion(w))
    t <- t+1
  }
  return(list("Solucion" = w, "Valor" = funcion(w), "Iteraciones" = t, "Imagenes" = imagenes))
}

## ----3a, echo=FALSE------------------------------------------------------
  resultado <- metodoNewton(f, sol_inicial = c(0.1,0.1), max_iter = 50, tasa_aprendizaje = 0.01)
  resultado2 <- metodoNewton(f, sol_inicial = c(1,1), max_iter = 50, tasa_aprendizaje = 0.01)
  resultado3 <- metodoNewton(f, sol_inicial = c(-0.5,-0.5), max_iter = 50, tasa_aprendizaje = 0.01)
  resultado4 <- metodoNewton(f, sol_inicial = c(-1,-1), max_iter = 50, tasa_aprendizaje = 0.01)

  l1 <- length(resultado$Imagenes)
  l2 <- length(resultado2$Imagenes)
  l3 <- length(resultado3$Imagenes)
  l4 <- length(resultado4$Imagenes)
  
  datos <- data.frame(Iteraciones = 1:50,
         Imagen1 = c(resultado$Imagenes, rep(resultado$Imagenes[l1-1],  max(50 - l1,0)) ),
         Imagen2 = c(resultado2$Imagenes,rep(resultado2$Imagenes[l2-1], max(50 - l2,0)) ),
         Imagen3 = c(resultado3$Imagenes,rep(resultado3$Imagenes[l3-1], max(50 - l3,0)) ),
         Imagen4 = c(resultado4$Imagenes,rep(resultado4$Imagenes[l4-1], max(50 - l4,0)) )
  )
  
  ggplot(data=datos, aes(x = Iteraciones)) +
    geom_line(aes(y=Imagen1), colour = "blue") + 
    geom_line(aes(y=Imagen2), colour = "green") +
    geom_line(aes(y=Imagen3), colour = "red") + 
    geom_line(aes(y=Imagen4), colour = "black") +
    labs(list(title = "Evolución de imágenes", x = "Iteraciones", y = "Imagen")) 

  print("Método de Newton")
  pausa()
  
## ----4, echo=FALSE-------------------------------------------------------
datos <- simula_unif(N = 100, rango = c(-1,1))
recta <- simula_recta(c(-1,1))

datos.etiquetados <- cbind(datos, Etiqueta = sign(datos$Y - recta[2]*datos$X - recta[1])/2 + 1/2)

ggplot(data=datos.etiquetados, aes(x = X, y = Y)) + 
  ggtitle("Datos clasificados con función estimada") +
  geom_point(aes(colour = factor(Etiqueta))) +
  geom_abline(intercept = recta[1], slope = recta[2]) +
  theme(legend.title=element_blank())

pausa()

## ----gradienteDescendenteEstocastico-------------------------------------
regresionLogistica <- function(datos, etiquetas, sol_inicial = rep(0,3), tol = 0.01, tasa_aprendizaje = 0.01) {
  w.actual <- sol_inicial
  t <- 0
  datos <- cbind(1,datos)
  # datos <- cbind(datos,1)

  while (t == 0 || distance(w.anterior, w.actual) > tol){
    t <-  t+1
    w.anterior <- w.actual
    
    # Aleatorizamos las etiquetas
    permutacion <- sample(nrow(etiquetas))

    # Recorremos las muestras
    for(idx in permutacion) {
      x_i <- as.numeric(datos[idx,])
      y_i <- as.numeric(etiquetas[idx,])
      
      g_t <- -(y_i*x_i)/(1+exp(y_i*crossprod(w.actual,x_i)))

      w.actual <- w.actual - tasa_aprendizaje*g_t
    }
  }
  return(list("Pesos" = w.actual, "Recta" = c(-w.actual[1]/w.actual[3], -w.actual[2]/w.actual[3]), "Iteraciones" = t))
}

## ----4b------------------------------------------------------------------
# datos.etiquetados
resultado <- regresionLogistica(datos, datos.etiquetados["Etiqueta"])
recta.regresion <- resultado$Recta

ggplot(data=datos.etiquetados, aes(x = X, y = Y), colour = factor(Etiqueta)) + 
  ggtitle("Datos etiquetados con regresión logística") +
  geom_point(aes(colour = factor(Etiqueta))) +
  geom_abline(intercept = recta[1], slope = recta[2], color = "red") +
  geom_abline(intercept = recta.regresion[1], slope = recta.regresion[2], color = "blue") +
  theme(legend.title=element_blank())

## ----4b2-----------------------------------------------------------------
tamanio = 1000
datos.test <- simula_unif(N = tamanio, d = 2, rango = c(-1,1))
datos.test.etiquetas <- sign(datos.test$Y - recta[2]*datos.test$X - recta[1])/2 + 1/2
datos.test.etiquetas.regresion <- sign(datos.test$Y - recta.regresion[2]*datos.test$X - recta.regresion[1])/2 + 1/2
Eout <- sum(datos.test.etiquetas != datos.test.etiquetas) / tamanio
print("Regresión logística")
print(paste("El Eout es",Eout))
pausa()
## ----5-------------------------------------------------------------------
digitos.train <- read.table("datos/zip.train")
digitos.test <- read.table("datos/zip.test")

digitos.train <- digitos.train[ which(digitos.train$V1 == 1 | digitos.train$V1 == 5), ]
digitos.test <- digitos.test[ which(digitos.test$V1 == 1 | digitos.test$V1 == 5), ]

digitos.train.matrix <- lapply(split(data.matrix(digitos.train[,-1]), 1:nrow(digitos.train)), 
                           function(m) { 
                             mtx <- matrix(0.5*(1-m), nrow = 16, ncol = 16)
                             return(mtx[,16:1])
                             })
digitos.test.matrix <- lapply(split(data.matrix(digitos.test[,-1]), 1:nrow(digitos.test)),
                           function(m) {
                             mtx <- matrix(0.5*(1-m), nrow = 16, ncol = 16)
                             return(mtx[,16:1])
                             })

medias.train <- as.numeric(lapply(digitos.train.matrix, mean))
medias.test <- as.numeric(lapply(digitos.test.matrix, mean))

simetriaVertical <- function(vector) {
  simetria <- 2*sum(abs(vector[1:(length(vector)/2)] - vector[(length(vector)/2+1):length(vector)]))
  return(simetria)
}

simetrias.train <- as.numeric(lapply(digitos.train.matrix,function(m) -sum(apply(m,1,simetriaVertical)) ))
simetrias.test <- as.numeric(lapply(digitos.test.matrix,function(m) -sum(apply(m,1,simetriaVertical)) ))

datos.digitos.train <- data.frame(Etiqueta = digitos.train$V1,
                                  Media = medias.train,
                                  Simetria = simetrias.train)

datos.digitos.test <- data.frame(Etiqueta = digitos.test$V1,
                                  Media = medias.test,
                                  Simetria = simetrias.test)

ggplot(data=datos.digitos.train, aes(x = Media, y = Simetria)) +
  ggtitle("Datos de entrenamiento") +
  geom_point(aes(colour = factor(Etiqueta))) +
  theme(legend.title=element_blank())
  
pausa()

ggplot(data=datos.digitos.test, aes(x = Media, y = Simetria)) +
  ggtitle("Datos de test") +
  geom_point(aes(colour = factor(Etiqueta))) +
  theme(legend.title=element_blank())

pausa()
## ----5a------------------------------------------------------------------
recta.regresion.digitos <- regres_lin(datos.digitos.train[,c("Media", "Simetria")], 
                                      datos.digitos.train[,c("Etiqueta")] * (-1/2) + 3/2,
                                      pesos = TRUE)

recta.regresion.digitos.mejorado <- ajusta_PLA_MOD(datos.digitos.train[,c("Media", "Simetria")], 
                                      datos.digitos.train[,c("Etiqueta")] * (-1/2) + 3/2,
                                      max_iter = 10,
                                      vini = recta.regresion.digitos)

recta.regresion.digitos.coeficientes <- recta.regresion.digitos.mejorado$Recta

ggplot(data=datos.digitos.train, aes(x = Media, y = Simetria)) +
  ggtitle("Clasificación en datos de entrenamiento") +
  geom_point(aes(colour = factor(Etiqueta))) +
  geom_abline(intercept = recta.regresion.digitos.coeficientes[1], slope = recta.regresion.digitos.coeficientes[2], 
              color = "yellow") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_discrete(name="Números manuscritos")

pausa()

ggplot(data=datos.digitos.test, aes(x = Media, y = Simetria)) +
  ggtitle("Clasificación en datos de test") +
  geom_point(aes(colour = factor(Etiqueta))) +
  geom_abline(intercept = recta.regresion.digitos.coeficientes[1], slope = recta.regresion.digitos.coeficientes[2], 
              color = "yellow") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_discrete(name="Números manuscritos")


## ----5b, include=FALSE---------------------------------------------------
Ein <- recta.regresion.digitos.mejorado$Errores / nrow(datos.digitos.train)
Etest <- sum(apply(cbind(1,datos.digitos.test[,c("Media","Simetria")]), 
                   1, 
                   function(x) { sign(crossprod(recta.regresion.digitos.mejorado$Pesos,as.numeric(x))) } 
              ) != 
               datos.digitos.test[,c("Etiqueta")] * (-1/2) + 3/2 ) /
            nrow(datos.digitos.test)

## ----cotasVC-------------------------------------------------------------
N <- nrow(datos.digitos.train)
delta <- 0.05
dVC <- 3
Eout.in <- Ein + sqrt((8/N) * log((4*(2*N)^dVC+4)/delta))

## ----cotaHoeffding-------------------------------------------------------
N <- nrow(datos.digitos.test)
delta <- 0.05
epsilon = sqrt(-log(delta/2)/(2*nrow(datos.digitos.test)))
Eout.test <- Etest + epsilon

print(paste("El error fuera de la muestra generalizando con Ein es",Eout.in))
print(paste("El error fuera de la muestra generalizando con Etest es",Eout.test))

pausa()
## ----Legendre------------------------------------------------------------
legendre <- function(k,x){
  resultado <- c(1,x)
  grado <- 3

  while(grado <= k+1){
    resultado[grado] <- ((2*(grado-1) - 1) / (grado-1)) * 
                  x * resultado[grado-1] - 
                  (((grado-1) - 1) / (grado-1)) * resultado[grado-2]
    grado <- grado+1
  }

  return(resultado[k+1])
}

## ----defPrevia-----------------------------------------------------------
Qf <- 20
N <- 50
sigma <- 1

## ----aq------------------------------------------------------------------
divisor <- sqrt(sum(sapply(0:Qf, function(q) {1 / (2*q+1)})))
a_q <- rnorm(Qf+1) / divisor

## ----sumaLegendre--------------------------------------------------------
legendre.suma <- function(x) {
  suma <- 0
  for (i in 0:Qf){
    suma <- suma + a_q[i+1] * legendre(i,x)
  }
  return(suma)
}

legendre.ruido <- rnorm(N)
legendre.x <- runif(N,-1,1)
legendre.y <- sapply(legendre.x, legendre.suma) + sigma*legendre.ruido
legendre.df <- data.frame(X = legendre.x, Y = legendre.y)

ggplot(legendre.df, aes(X,Y)) +
  ggtitle("Puntos para estudiar sobreajuste") +
  geom_point()

print("Puntos para ver sobreajuste")
pausa()

## ----transformacionH2H10-------------------------------------------------
legendre.datos.h2 <- t(sapply(legendre.x, function(x) { sapply(1:2, function(n) {x^n})}))
legendre.datos.h10 <- t(sapply(legendre.x, function(x) { sapply(1:10, function(n) {x^n})}))

## ----g2g10---------------------------------------------------------------
legendre.g2.pesos <- regres_lin(legendre.datos.h2, legendre.y, pesos=TRUE)
legendre.g10.pesos <- regres_lin(legendre.datos.h10, legendre.y, pesos=TRUE)

legendre.g2.f <- function(x, l=legendre.g2.pesos) {l[1] + l[2]*x + l[3]*x^2}
legendre.g10.f <- function(x, l=legendre.g10.pesos) {l[1] + l[2]*x + l[3]*x^2 + l[4]*x^3 + l[5]*x^4 + l[6]*x^5
                              + l[7]*x^6 + l[8]*x^7 + l[9]*x^8 + l[10]*x^9 + l[11]*x^10}

legendre.g2.imagen <- sapply(legendre.x, legendre.g2.f )
legendre.g10.imagen <- sapply(legendre.x, legendre.g10.f)

legendre.f.imagen <- sapply(legendre.x, legendre.suma)

legendre.gx.datos <- data.frame(X = legendre.x,
                                M = legendre.y,
                                Y = legendre.f.imagen,
                                Y2 = legendre.g2.imagen,
                                Y10 = legendre.g10.imagen)

ggplot(legendre.gx.datos, aes(x = X)) +
  ggtitle("Ajuste usando polinomios de Legendre") +
  geom_line(aes(y = Y), colour = "red") +
  geom_line(aes(y = Y2), colour = "blue") +
  geom_line(aes(y = Y10), colour = "green") +
  geom_point(aes(y = M), colour = "coral") +
  coord_cartesian(ylim = c(-10, 10)) 

print("Ajustes usando polinomios de Legendre")
## ----MSE-----------------------------------------------------------------
num_muestras <- 1000
legendre.test.x <- runif(num_muestras, -1, 1)
legendre.test.y <- sapply(legendre.test.x, legendre.suma)

legendre.test.g2 <- sapply(legendre.test.x, legendre.g2.f)
legendre.test.g10 <- sapply(legendre.test.x, legendre.g10.f)

legendre.g2.eout <- sum((legendre.test.y - legendre.test.g2)^2) / num_muestras
legendre.g10.eout <- sum((legendre.test.y - legendre.test.g10)^2) / num_muestras

print(paste("El MSE de g2 es",legendre.g2.eout))
print(paste("El MSE de g10 es",legendre.g10.eout))
pausa()
graphics.off()