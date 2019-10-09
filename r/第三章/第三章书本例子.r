#-
library(magrittr)
#######
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5, 66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
w.mean <- mean(w)
x <- 1:12; dim(x)<-c(3,4);mean(x)
apply(x, 1, mean)
apply(x, 2, mean)
mean(as.data.frame(x))
w.na <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5, 66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0, NA)
w.mean <- mean(w.na);w.mean
w.mean <- mean(w.na,na.rm = TRUE)
#---
x <- c(75, 64, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5)
sort(x)
sort(x,decreasing = TRUE)
x.na <- c(75.0,64.0,47.4,NA,66.9,62.2,62.2,58.7,63.5)
sort(x.na)
sort(x.na,na.last = TRUE)
median(x.na)
median(x.na,na.rm = TRUE)
#----
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5, 66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
quantile(w)
#--
var(w)
sd(w)
#--
data_outline <- function(x){
  n <- length(x)
  m <- mean(x)
  v <- var(x)
  s <- sd(x)
  me <- median(x)
  cv <- 100*s/m
  css <- sum((x-m)^2)
  uss <- sum(x^2)
  R <-  max(x)-min(x)
  R1 <- quantile(x,3/4)-quantile(x,1/4)
  sm <- s/sqrt(n)
  g1 <- n/((n-1)*(n-2))*sum((x-m)^3)/s^3
  g2 <- ((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4
         - (3*(n-1)^2)/((n-2)*(n-3)))
  data.frame(N=n, Mean=m, Var=v, std_dev=s, Median=me, 
             std_mean=sm, CV=cv, CSS=css, USS=uss, R=R, 
             R1=R1, Skewness=g1, Kurtosis=g2, row.names=1)
}

w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
data_outline(w)
#---
r <- rnorm(100,0,1)
hist(r)
density(r)

#_--
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5, 66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
hist(w, freq = FALSE)
lines(density(w), col = "blue")
x <- 44:76
lines(x, dnorm(x, mean(w), sd(w)), col = "red")

#---
plot(ecdf(w),verticals = TRUE, do.p = FALSE) 
x <- 44:78
lines(x,pnorm(x,mean = mean(w),sd(w)))
#--
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
qqnorm(w); qqline(w)
#--
x<-c(25, 45, 50, 54, 55, 61, 64, 68, 72, 75, 75, 78, 79, 81, 83, 84, 84, 84, 85, 86, 86, 86, 87, 89, 89, 89, 90, 91, 91, 92, 100)
stem(x)
stem(x, scale = 2)
#--
boxplot(x)
#---
x<-c(25, 45, 50, 54, 55, 61, 64, 68, 72, 75, 75, 
     78, 79, 81, 83, 84, 84, 84, 85, 86, 86, 86, 
     87, 89, 89, 89, 90, 91, 91, 92, 100)
fivenum(x)
#--
y<-c(1600, 1610, 1650, 1680, 1700, 1700, 1780, 1500, 1640, 
     1400, 1700, 1750, 1640, 1550, 1600, 1620, 1640, 1600, 
     1740, 1800, 1510, 1520, 1530, 1570, 1640, 1600)
f<-factor(c(rep(1,7),rep(2,5), rep(3,8), rep(4,6)))
plot(f,y)
#---
x<-y<-seq(-2*pi, 2*pi, pi/15)
f<-function(x,y) sin(x)*sin(y)
z<-outer(x,y, f)
contour(x,y,z,col="blue")
persp(x,y,z,theta=45, phi=20, expand=1,col="lightblue")
#-----
rt <- read.table("薛毅/统计建模与R代码及答案/Ch03/exam0203.txt",head = TRUE)
lm.sol <- lm(Weight~Height,data = rt)
attach(rt)
plot(Weight~Height);abline(lm.sol)
ggplot(data=rt,aes(x=Height,y=Weight))+
  geom_smooth(method='lm')+
  geom_point()
#-----
ore<-data.frame(
  x=c(67, 54, 72, 64, 39, 22, 58, 43, 46, 34),
  y=c(24, 15, 23, 19, 16, 11, 20, 16.1, 17, 13)
)
ore.m <- apply(ore, 2, mean);ore.m
ore.s<-cov(ore); ore.s
ore.r<-cor(ore); ore.r
attach(ore)
cor.test(x,y)
cor.test(x,y, method="spearman")
cor.test(x,y, method="kendall")
#----
  ruben.test<-function(n, r, alpha=0.05){
  u<-qnorm(1-alpha/2)
  r_star<-r/sqrt(1-r^2)
  a<-2*n-3-u^2; b<-r_star*sqrt((2*n-3)*(2*n-5))
  c<-(2*n-5-u^2)*r_star^2-2*u^2
  y1<-(b-sqrt(b^2-a*c))/a
  y2<-(b+sqrt(b^2-a*c))/a
  data.frame(n=n, r=r, conf=1-alpha, 
             L=y1/sqrt(1+y1^2), U=y2/sqrt(1+y2^2))
}
ruben.test(6,0.8)
#----
rubber <- read.table("薛毅/统计建模与R代码及答案/Ch03/rubber.data",head = TRUE)
apply(rubber, 2, mean)
cov(rubber)
cor(rubber)
cor.test(~X1+X2,data = rubber)
cor.test(~X1+X3,data = rubber)
cor.test(~X2+X3,data = rubber)
#---
rt <- read.table("薛毅/统计建模与R代码及答案/Ch03/applicant.data",head = TRUE)
AVG <- apply(rt, 1, mean)
sort(AVG,decreasing = TRUE)
cor(rt)
attach(rt)
rt$G1 <- (SC+LC+SMS+DRV+AMB+GSP+POT)/7
rt$G2 <- (FL+EXP+SUIT)/3
rt$G3 <- (LA+HON+KJ)/3
rt$G4 <- AA
rt$G5 <- APP
AVG <- apply(rt[,16:20], 1, mean)
sort(AVG, decreasing = TRUE)
#---
X <- read.table("薛毅/统计建模与R代码及答案/Ch03/course.data",head = TRUE)
source("薛毅/统计建模与R代码及答案/Ch03/outline.R")
outline(X)
stars(X)
