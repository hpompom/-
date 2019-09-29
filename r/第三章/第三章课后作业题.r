library(pastecs)
library(ggplot2)
library(DescTools)
library(magrittr)
library(qqman)
library(grDevices) 
library(shape)
#----3.1-----
data.3_1<-c(74.3,78.8,68.8,78.0,70.4,80.5,80.5,69.7,71.2,73.5,
     79.5,75.6,75.0,78.8,72.0,72.0,72.0,74.3,71.2,72.0,
     75.0,73.5,78.8,74.3,75.8,65.0,74.3,71.2,69.7,68.0,
     73.5,75.0,72.0,64.3,75.8,80.3,69.7,74.3,73.5,73.5,
     75.8,75.8,68.8,76.5,70.4,71.2,81.2,75.0,70.4,68.0,
     70.4,72.0,76.5,74.3,76.5,77.6,67.3,72.0,75.0,74.3,
     73.5,79.5,73.5,74.7,65.0,76.5,81.6,75.4,72.7,72.7,
     67.2,76.5,72.7,70.4,77.2,68.8,67.3,67.3,67.3,72.7,
     75.8,73.5,75.0,73.5,73.5,73.5,72.7,81.6,70.3,74.3,
     73.5,79.5,70.4,76.5,72.7,77.2,84.3,75.0,76.5,70.4)
stat.desc(data.3_1)[c(9,12,13)]#mean、var、std
Skew(data.3_1)
Kurt(data.3_1)
#----3.2-----
data.3_1.gg <- data.frame(c(1:100),data.3_1)
colnames(data.3_1.gg) <- c("X","data")
ggplot(data.3_1.gg,aes(x=data))+
  geom_histogram(binwidth=1,fill=NA, color="black")
ggplot(data.3_1.gg,aes(x=data))+
  geom_density()
ggplot(data.3_1.gg,aes(x=data))+
  stat_ecdf()
ggplot(data.3_1.gg,aes(sample = data))+
  stat_qq() +
  stat_qq_line()
ggplot(data.3_1.gg,aes(x=data))+
  geom_density(color = 'red')+
  geom_density(aes(rnorm(100,mean = mean(data.3_1),sd = sd(data.3_1))))
#----3.3-----
boxplot(data.3_1)
stem(data.3_1)
boxplot(data.3_1)$stats
#-----3.4-----
shapiro.test(data.3_1)
ks.test(data.3_1,rnorm(100,mean = mean(data.3_1),sd = sd(data.3_1)))
#----3.5-----
#用plot
x<-c(2,4,3,2,4,7,7,2,2,5,4,5,6,8,5,10,7,12,12,6,6,7,11,6,6,7,9,5,5,10,6,3,10)
f<-factor(c(rep(1,11),rep(2,10),rep(3,12)))
plot(f,x)
#用boxplot
A1<-c(2,4,3,2,4,7,7,2,2,5,4)
A2<-c(5,6,8,5,10,7,12,12,6,6)
A3<-c(7,11,6,6,7,9,5,5,10,6,3,10)
boxplot(A1,A2,A3)
#----3.6----
x<-c(65,45,27.6,
     70,45,30.7,
     70,48,31.8,
     69,46,32.6,
     66,50,31.0,
     67,46,31.3,
     68,47,37.0,
     72,43,33.6,
     66,47,33.1,
     68,48,34.2)
plot(x)
#----3.7-----
data.3_7 <- read.table("薛毅/统计建模与R代码及答案/3/3.7.txt")
data.3_7
colnames(data.3_7) <- c("number","name","sex","age","height","weight")
attach(data.3_7)
coplot(weight~height|sex,col="blue")
coplot(weight~height|age,col="black")
coplot(weight~height|sex+age,col="darkgreen")
#----3.8----
x<-seq(-2,3,0.5)
y<-seq(-1,7,0.5)
f<-function(x,y) x^4-2*x^2*y+x^2-2*x*y+2*y^2+4.5*x-4*y+4
z<-outer(x,y,f)
persp(x, y, z , theta = 55, phi = 45
      ,xlab = "X", ylab = "Y", zlab = "Z", 
      col = drapecol(z) ,ltheta = 120, shade = 0.75, ticktype = "detailed"
)
contour(x,y,z,levels=c(0,1,2,3,4,5,10,15,20,30,40,50,60,80,100),col="blue")

#---3.9----
cor.test(weight,height,alternative="two.sided",method="pearson")
#-----3.10----
a<-read.table("薛毅/统计建模与R代码及答案/3/3.17.txt")%>%
  data.frame()

stars(a)
attach(a)
#1
stars(a)
#2
G1 <- (SC+LC+SMS+DRV+AMB+GSP+POT)/7
G2 <- (FL+EXP+SUIT)/3
G3 <- (LA+HON+KJ)/3
G4 <- AA
G5 <- APP
b <- cbind(G1,G2,G3,G4,G5)
stars(b)
#-----3.11-----
library(MSG)
andrews_curve(b)


