library(tidyverse)



step <- function(x){
  delta <- .01
  A <- matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1, 1,0,0,0), ncol=4) - diag(1,4,4)
  x+delta*t(t(x)  %*%  A)
}

n <- 4
x  <- matrix(c(1, 0, -1,  0,
               0, 1,  0, -1), ncol=2)

 
n.steps <- 1000
for(i in seq(0,(n.steps-2)*4,by=4)) 
  x <- rbind(x, step(x[seq(i+1,i+4),],4))

x <- cbind(x,rep(1:n,n.steps))



ggplot(data.frame(x=x[,1],y=x[,2], type=factor(x[,3]))) + 
  geom_point(aes(x,y, color=type), size=.1) + 
  scale_x_continuous(breaks=seq(-1,1,by=.2)) + 
  scale_y_continuous(breaks=seq(-1,1,by=.2)) +
  theme_minimal() + theme(legend.position = 0)

x.1 <- x[rep(c(TRUE,FALSE,FALSE,FALSE), nrow(x)/4),]
x.t.minus.first <- x.1[-1,]
x.t.minus.last  <- x.1[-nrow(x)/4,]
x.t <- x.t.minus.first - x.t.minus.last

sum(sqrt(x.t[,1]^2+x.t[,2]^2))
sqrt(2)

#######################################################



# This function rotates the the turtles. 
# Each turtle takes a single step towards the closest 
# neighbour anti-clockwise 
step <- function(x,n){
  delta <- .01
  A0 <- rbind(diag(1,n,n)[n,], diag(1,n,n)[-n,] )
  A <- A0 - diag(1,n,n)
  x+delta*t(t(x)  %*%  A)
}

# The number of turtles in the corners..
n <- 4
x <- matrix(c(cos(2*pi/n*seq(0,n-1,by=1)),
              sin(2*pi/n*seq(0,n-1,by=1))), ncol=2)

n.steps <- 10000
for(i in seq(0,(n.steps-2)*n,by=n)) 
  x <- rbind(x, step(x[seq(i+1,i+n),],n))
dim(x)

x <- cbind(x,rep(1:n,n.steps))

ggplot(data.frame(x=x[,1],y=x[,2], type=factor(x[,3]))) + 
  geom_point(aes(x,y, color=type), size=.1) + 
  scale_x_continuous(breaks=seq(-1,1,by=.2)) + 
  scale_y_continuous(breaks=seq(-1,1,by=.2)) +
  theme_minimal() + theme(legend.position = 0)

# Here we calculate the length of the first turtle's journey
x.1 <- x[rep(c(TRUE,rep(FALSE,n-1)), nrow(x)/n),]
x.t.minus.first <- x.1[-1,]
x.t.minus.last  <- x.1[-nrow(x)/n,]
x.t <- x.t.minus.first - x.t.minus.last

sum(sqrt(x.t[,1]^2+x.t[,2]^2))


