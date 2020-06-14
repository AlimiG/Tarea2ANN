######Perceptron####
library(ggplot2)
dot <- function(a,b){
  return(a%*%b)
}
umbral <- function(a){
  if (a > 0){
    return(1)
  }
  return(0)
}
# visualize the digits
show <- function(x,result,real){
  im <- matrix(x, ncol = 28, nrow = 28)
  image(im, col=gray((255:0)/255),xaxt='n',yaxt='n',cex=0.5,mar=c(0,4,4,2))
  title(main = c('predicted: ',result), sub = real)

}

perceptron <- function(train,eta,numneur,iter){
  x <- train[,2:785]
  #Inicializamos la matriz de pesos
  w <- rnorm(dim(x)[2]*numneur,0,0.25)
  w <- matrix(w,ncol=dim(x)[2],nrow=numneur)
  for (epoch in 1:iter) {
    train <- train[sample(nrow(train)),]
    y <- train[,1]
    x <- train[,2:785]
    x <- x/255
      for (j in 1:length(y)) {
        d <- which.max(dot(w,x[j,]))
        if (d != y[j]+1){
          w[y[j]+1,] = w[y[j]+1,]+eta*x[j,]
          w[d,] = w[d,]-eta*x[j,]
          
        }
        
      }
    
  }
  return(w)
}
test <- function(x,y,w){
  count <- 0
  for (i in 1:length(y)) {
    d <- which.max(dot(w,x[i,]))
    if (d-1 == (y[i])) {
      count <- count+1
    }
  }
  return(count/length(y))
  
}




train <- read.csv('mnist_train.csv')
train <- apply(train,2, as.numeric)

test_data <- read.csv('mnist_test.csv')
test_data <- apply(test_data,2, as.numeric)
y_d <- test_data[,1]


x_d <- test_data[,2:785]
x_d <- x_d/255
w <-perceptron(train,1/(10**2),10,50)

precition <- c()
elapsed <-c()
for (i in 1:3) {
  start_time <- Sys.time()
  w <-perceptron(train,1/(10**i),10,50)
  elapsed <-c(elapsed, Sys.time()- start_time)
  precition <- c(precition,test(x_d,y_d,w))
}



x <- c(0.001,0.01,0.1)
timeit <- data.frame(x,-1*elapsed)
precitionit <- data.frame(x,precition)



ggplot(timeit,aes(x,elapsed))+
  labs(title='Representacion Tiempo vs Constante de aprendizaje',x= 'eta',y='Tiempo de ejecucion (min)')+
  geom_line(color='Darkred',size=1)+
  geom_point(color='Darkred',size=4)+
  ylim(1.5,2.5)+
  theme_minimal()

ggplot(precitionit,aes(x,precition))+
  labs(title='Representacion Precision vs Constante de aprendizaje',x= 'eta',y='Precision')+
  geom_line(color='Darkred',size=1)+
  geom_point(color='Darkred',size=4)+
  ylim(0.8,1)
  theme_minimal()



par(mfrow=c(1,2))
for (i in sample(1:dim(x_d)[1], 2)) {
  show(x_d[i,],which.max(dot(w,x_d[i,]))-1,y_d[i])
}
test(x_d,y_d,w)



w <- rnorm(81,0,0.07)
w <- matrix(w,ncol=9,9)
w
start_time <- Sys.time()
dot(y_d,y_d)
end_time <- Sys.time()
