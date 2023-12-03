# ------------------------------------------------------------------------------
# --------------------------------- # Exp # ------------------------------------
# ------------------------------------------------------------------------------

f_obj_exp <- function(x, lambda){
  return(ifelse(x < 0, 0, lambda*exp(-lambda*x)))
}


mh_exp <- function(x_atual, n, lambda){
  x <- vector()
  x[1] <- x_atual
  for(i in c(2:n)){
    x_prop <- x_atual + rnorm(1, mean=0, sd=1)
    rho <- min(f_obj_exp(x_prop, lambda)/f_obj_exp(x_atual, lambda), 1)
    if(runif(1) < rho){
      x_atual <- x_prop
    }
    x[i] <- x_atual
  }
  return(x)
}

x_exp <- mh_exp(2, 10000, 1)

hist(x_exp, xlim=c(0,10), probability = TRUE, 
     main = "Histograma dos valores de x visitados pelo algoritmo MH", 
     xlab = "x",
     ylab = "Densidade")
pontos_exp = seq(0,10,length=100)
lines(pontos_exp, f_obj_exp(pontos_exp, 1),col="red")

y_exp <- vector()

for(i in c(1:1000)){
  x_0 <- runif(1, 10, 20)
  y_exp[i] <- mh_exp(x_0, 10000, 1)[10000]
}

hist(y_exp, xlim=c(0,10), probability = TRUE, 
     main = "Histograma dos valores de x visitados pelo algoritmo MH", 
     xlab = "x",
     ylab = "Densidade")
lines(pontos_exp, f_obj_exp(pontos_exp, 1), col="red")

# ------------------------------------------------------------------------------
# -------------------------------- # Normal # ----------------------------------
# ------------------------------------------------------------------------------

f_obj_norm <- function(x){
  return(1/sqrt(2*pi)*exp(-x^2/2))
}

mh_norm <- function(x_atual, n){
  x <- vector()
  x[1] <- x_atual
  for(i in c(2:n)){
    x_prop <- runif(1, x_atual-1, x_atual+1)
    rho <- min(exp(-(x_prop^2 - x_atual^2)/2), 1)
    if(runif(1) < rho){
      x_atual <- x_prop
    }
    x[i] <- x_atual
  }
  return(x)
}

x_norm <- mh_norm(0, 10000)

hist(x_norm, xlim=c(-5,5), probability = TRUE, 
     main = "Histograma dos valores de x visitados pelo algoritmo MH", 
     xlab = "x",
     ylab = "Densidade")
pontos_norm <- seq(-10,10, length=100)
lines(pontos_norm, f_obj_norm(pontos_norm), col="red")

y_norm <- vector()

for(i in c(1:1000)){
  x_0 <- 0
  y[i] <- mh_norm(x_0, 10000)[1000]
}


hist(y_norm, probability = TRUE, 
     main = "Histograma dos valores de x visitados pelo algoritmo MH", 
     xlab = "x",
     ylab = "Densidade")
lines(pontos_norm, f_obj_norm(pontos_norm),col="red")


# ------------------------------------------------------------------------------
# ----------------------------- # Normal Biv # ---------------------------------
# ------------------------------------------------------------------------------
library(mnormt)

rho <- -0.60
n <- 20000
sdev <- sqrt(1-rho^2)
bfx <- matrix(rep(0,2*n), ncol=2)
for (i in 2:n) {
  bfx[i,1] <- rnorm(1, rho*bfx[i-1,2], sdev)
  bfx[i,2] <- rnorm(1, rho*bfx[i,1], sdev)
}

x <- seq(-3, 3, 0.1) 
y <- seq(-3, 3, 0.1)
mu <- c(0, 0)
sigma <- matrix(c(1, rho, rho, 1), nrow=2)
f <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z <- outer(x, y, f)

contour(x, y, z)
plot(bfx, xlim = c(-3,3), ylim = c(-3,3), pch=20, xlab="x", ylab="y", main="")
