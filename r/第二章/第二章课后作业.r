#---2.1-----
x <- c(1,2,3)
y <- c(4,5,6)
e <- c(1,1,1)
z <- x+y+e
crossprod(x,y)
outer(x,y)#外积
#---2.2----
A <- matrix(1:20,nrow = 4,ncol = 5)
B <- matrix(1:20,nrow = 4,ncol = 5,byrow = TRUE)
C <- A+B
D <- A%*%B
E <- A*B
F <- A[1:3,1:3]
G <- B[,-3]
#---2.3----
x <- c(rep(1,times=5),rep(2,times=3),rep(3,times=4),rep(4,times=2))
#----2.4----
H <- matrix(ncol = 5,nrow = 5)
for (i in 1:5) {
  for (j in 1:5) {
    H[i,j] <- 1/(i+j-1)
  }
}
solve(H)
eigen(H)
#---2.5---
data.stu<-data.frame(序号=c(1:5),
                姓名=c("张三","李四","王五","赵六","丁一"),
                性别=c("女","男","女","男","女"),
                年龄=c(14,15,16,14,15),
                身高=c(156,165,157,162,159),
                体重=c(42,49,41.5,52,45.5))
write.csv(data,stu,file = "data.stu.csv")

fun.2.7 <- function(n){
  if(n<=0){
    print("要求输入一个正整数")
  }
  else{
    repeat{if(n==1) {
      break
    }
      else if(n%%2==0){
        n<-n/2
      }
      else {n<-3*n+1
      }
    }
    print("运算成功")
  }
}
