#--载入包-----
library(magrittr)
library(bruceR)
library(ggplot2)
###############
X1 <- c(35, 40, 40, 42, 37, 45, 43, 37, 44, 42, 41, 39)%T>%
  hist()
mean(X1);sd(X1)
X2 <- c(60, 74, 64, 71, 72, 68, 78, 66, 70, 65, 73, 75)
mean(X2);sd(X2)
plot(X1,X2)
#--线性回归----
exam0203 <- read.csv("exam0203.txt", sep="")
colnames(exam0203)
lm.model <- lm(Weight~Height,data = exam0203)
summary(lm.model)
GLM_summary(lm.model)
#--复数向量-----
x <- seq(-pi, pi, by=pi/10)
y <- sin(x)
z <- complex(re=x, im=y)
z.p <- data.frame(x,y)
ggplot(data=z.p,aes(x=x,y=y))+
  geom_line()+
  geom_point()
#---最小拟合与QR分解-----
##------最小拟合------
x<-c(0.0, 0.2, 0.4, 0.6, 0.8)
y <- c(.9,1.9,2.8,3.3,4.2)
lsfit.sol <- lsfit(x,y)#最小拟合
#-----求矩阵特征值和特征向量-------
Sm<-crossprod(A,A)
ev<-eigen(Sm); ev
#----矩阵奇异值分解-----
svdA<-svd(A); svdA
##-----QR分解------
X<-matrix(c(rep(1,5), x), ncol=2)
Xplus <- qr(X)
b <- qr.coef(Xplus,y)#利用QR分解结果计算最小二乘系数
#--apply函数----
A<-matrix(1:6,nrow=2); A
apply(A,1,mean)
apply(A, 2, sd)
#------t检验----—
A <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
B <- c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)
t.test(A,B,var.equal = TRUE)#方差相等的t检验
#--------二分法--------
fzero <- function(f, a, b, eps=1e-5){ if (f(a)*f(b)>0)
  list(fail="finding root is fail!") else{
    repeat {
      if (abs(b-a)<eps) break
      x <- (a+b)/2
      if (f(a)*f(x)<0) b<-x else a<-x
    }
    list(root=(a+b)/2, fun=f(x))
  }
}
##-----求根的线性函数-----
f <- function(x) x^3-x-1
fzero(f,1,2,1e-6)
uniroot(f,c(1,2))#r自带
#----解线性方程组------
A <- t(array(c(1:8, 10),dim=c(3,3)))
b <- c(1,1,1)
x <- solve(A,b); x
B <- solve(A); B
#----定义新的二元运算-----
"%!%" <- function(x, y){
  exp(-0.5*(x-y) %*% (x-y))
  }
#-----牛顿迭代法------------
##----牛顿迭代法函数-------
Newtons<-function (fun, x, ep=1e-5, it_max=100){
  index<-0; k<-1
  while (k<=it_max){
    x1 <- x; obj <- fun(x);
    x  <- x - solve(obj$J, obj$f);
    norm <- sqrt((x-x1) %*% (x-x1))
    if (norm<ep){
      index<-1; break
    }
    k<-k+1
  }
  obj <- fun(x);
  list(root=x, it=k, index=index, FunVal= obj$f)
}
##---目标方程-----------
funs<-function(x){
  f<-c(x[1]^2+x[2]^2-5, (x[1]+1)*x[2]-(3*x[1]+1))
  J<-matrix(c(2*x[1], 2*x[2], x[2]-3, x[1]+1), 
            nrow=2, byrow=T)
  list(f=f, J=J)
}
#-----牛顿迭代法计算-------
Newtons(funs,c(0,1))
#----递归函数-----
area <- function(f, a, b, eps = 1.0e-06, lim = 10) {
  fun1 <- function(f, a, b, fa, fb, a0, eps, lim, fun) {
    d <- (a + b)/2; h <- (b - a)/4; fd <- f(d)
    a1 <- h * (fa + fd); a2 <- h * (fd + fb)
    if(abs(a0 - a1 - a2) < eps || lim == 0)
      return(a1 + a2)
    else {
      return(fun(f, a, d, fa, fd, a1, eps, lim - 1, fun)
             + fun(f, d, b, fd, fb, a2, eps, lim - 1, fun))
    }
  }
  fa <- f(a); fb <- f(b); a0 <- ((fa + fb) * (b - a))/2
  fun1(f, a, b, fa, fb, a0, eps, lim, fun1)
}
##定义函数
f <- function(x) 1/x
#计算积分值
quad<-area(f,1,5)
