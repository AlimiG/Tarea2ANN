#Demostracion

X <- matrix(c(2,6,1,1,3,1,3,9,1),nrow=3,ncol=3)
X
Y <- matrix(c(0,1,1),nrow=3,ncol=1)
Y

dot <- function(a,b){
  return(a%*%b)
}

umbral <- function(a){
  if (a > 0){
    return(1)
  }
  return(0)
}

w <- W_i
for (j in 1:iter) {
  for (i in 1:length(y)) {
    d <- umbral(dot(X[,i],W_i))
    
    w <- w + eta*(y[i]-d)*X[,i]
  }
  
}

perceptron <- function(x,y,eta,iter){
  W <- rep(0, 3)
  errors <- rep(0,iter)
  for (j in 1:iter) {
    for (i in 1:length(y)) {
      d <- umbral(dot(X[,i],w))
      
      w <- w + eta*(y[i]-d)*X[,i]

      if ((y[i] - d) != 0.0) {
        errors[j] <- errors[j] + 1
      }
    }
    
  }
  print(w)
  return(errors)
}

err <- perceptron(X,Y,1,50)


plot(1:50, err, type="l", lwd=2, col="red", xlab="epoca", ylab="errores")
