
##########【输出设置】##########
rm(list=ls())                   #清理内存
options(digits=4)               #输出结果位数
par(mar=c(4,4,2,1)+0.1,cex=0.75) #图形修饰

getwd()
#setwd("/Users/fangsha/Desktop/多元/上课ppt/2016_9_27/2多元数据的直观表示及R使用")
#getwd()


########################################
#  数据集 data.csv 来自于《多元统计分析和R语言建模》 Chapter 3
########################################
## 变量的解释:
#食品：人均食品支出；
#衣着：人均衣着商品支出；
#设备：人均家庭设备用品及服务支出；
#医疗：人均医疗保健支出；
#交通：人均交通和通信支出；
#教育：人均娱乐教育文化服务支出；
#居住：人均居住支出；
#杂项：人均杂项商品和服务支出。
########################################
library(magrittr)
library(bruceR)
# 整理数据
data <- read.csv("2019_10_23_computer多元/3多元正态分布Rcode/练习/data.csv",header=T)
head(data)#显示前6组数据
data[,1]
data <- data.frame(data[,-1], row.names = data[,1])
head(data)
dim(data)
cor(data)
#复相关系数
#Y <- data[,6]
#X <- data.frame(data[,-6])
#cor(Y, X)
#cor(X, Y)  
#mcor_Y_X <- sqrt(t(cor(X, Y)) %*% solve(cor(X, X)) %*% cor(X, Y)) 
#mcor_Y_X
#-
lm.model1 <- lm(data[,4]~data[,1]+data[,2]+data[,3]+data[,5]+data[,6]+data[,7]+data[,8])
plot(lm.model1)
cor(lm.model1$fitted.values,data[,4])

#--------#
#设备：3 医疗：4 杂项：8
pc <- function(x){
lm.model <- lm(x~data[,3]+data[,4]+data[,8])
return(lm.model$residuals)
}
apply(data[,-c(3,4,8)], 2, pc)%>%
  cor()




