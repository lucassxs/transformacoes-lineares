f1 <- function(x) dunif(x)
f2 <- function(x) dunif(x)


fy <- function(y1, y2) {
  if(y1+y2>=0 & y1+y2<=2 & y1-y2>0 & y1-y2<2) {
    x1 <- (y1+y2)/2
    x2 <- (y1-y2)/2
    out <- f1(x1)*f2(x2)/2
    } else {out <- 0}
  return(out)
  }


#yy1 <- seq(-2, 2, 0.01)
#yy2 <- seq(-2, 2, 0.01)
#n <- length(yy1)
#ff <- matrix(0, ncol=n, nrow=n)
#for(i in 1:n) {
#  for(j in 1:n) {
#    ff[i,j] <- fy(yy1[i], yy2[j])
#  }}

library(emdbook)
library(rgl)

curve3d(fy, from=c(0,-1), to=c(2,1), n=c(200, 200), sys3d="rgl", col='blue', 
xlab='y1', ylab='y2', zlab='f(y1, y2)')



f1 <- function(x) dexp(x, 2) #dunif(x)
f2 <- function(x) dexp(x, 4) #dunif(x)

fy <- function(y1, y2) {
  if(y1+y2>=0 & y1-y2>0) {
    x1 <- (y1+y2)/2
    x2 <- (y1-y2)/2
    out <- f1(x1)*f2(x2)/2
    } else {out <- 0}
  return(out)
  }
curve3d(fy, from=c(0,-10), to=c(10,10), n=c(200, 200), sys3d="contour", col='blue', 
xlab='y1', ylab='y2', zlab='f(y1, y2)')
