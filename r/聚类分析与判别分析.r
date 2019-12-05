library(readxl)#读入excel
library(ggplot2)#画图
library(MASS)#判别
library(ggord)#画图
library(data.table)#
library(PerformanceAnalytics)#cor
library(colorspace) # 颜色
library(factoextra)#k-means
library(dendextend)#
library(psych)#描述
require(gridExtra)#多图合并

country <- read_excel("~/Desktop/大三代码/多元大作业/country.xlsx")
#-----探索性数据分析————————

species_labels <-country$...8
data.1 <- country[,-c(1,8)]
length(country$Country)

species_col <- rev(rainbow_hcl(4))[as.numeric(species_labels)]
pairs(scale(data.1), col = species_col,
      lower.panel = NULL,
      cex.labels=1.1, pch=19, cex = 1.0)
par(xpd = TRUE)
legend(x = 0.05, y = 0.4, cex = 2,
       legend = as.character(levels(species_labels)),
       fill = unique(species_col))
par(xpd = NA)
#----#
chart.Correlation(data.1, histogram=TRUE, pch=19)
describe(country[,-c(1,8)])
fwrite(describe(country[,-c(1,8)]),"des.csv")

p1<-ggplot(data=country[,-1], aes(x=...8 ,y=GDP))+
  geom_boxplot(aes(fill=...8))+
  theme(legend.position = 'none', title = element_text(family = 'STKaiti'), # 设置不要旁边的那个解释框
        axis.title.x = element_text(family = 'STKaiti'), # 字体大小格式设置
        axis.title.y = element_text(family = 'STKaiti', size = 12, face = 'bold'),
        axis.text.x = element_text(family = 'STKaiti', size = 12, face = 'bold'))+
  labs(x="",y="GDP", main = "")
p2<-ggplot(data=country[,-1], aes(x=...8 ,y=import))+
  geom_boxplot(aes(fill=...8))+
  theme(legend.position = 'none', title = element_text(family = 'STKaiti'), # 设置不要旁边的那个解释框
        axis.title.x = element_text(family = 'STKaiti'), # 字体大小格式设置
        axis.title.y = element_text(family = 'STKaiti', size = 12, face = 'bold'),
        axis.text.x = element_text(family = 'STKaiti', size = 12, face = 'bold'))+
  labs(x="",y="import", main = "")
p3<-ggplot(data=country[,-1], aes(x=...8 ,y=export))+
  geom_boxplot(aes(fill=...8))+
  theme(legend.position = 'none', title = element_text(family = 'STKaiti'), # 设置不要旁边的那个解释框
        axis.title.x = element_text(family = 'STKaiti'), # 字体大小格式设置
        axis.title.y = element_text(family = 'STKaiti', size = 12, face = 'bold'),
        axis.text.x = element_text(family = 'STKaiti', size = 12, face = 'bold'))+
  labs(x="",y="export", main = "")
p4<-ggplot(data=country[,-1], aes(x=...8 ,y=`General government revenue`))+
  geom_boxplot(aes(fill=...8))+
  theme(legend.position = 'none', title = element_text(family = 'STKaiti'), # 设置不要旁边的那个解释框
        axis.title.x = element_text(family = 'STKaiti'), # 字体大小格式设置
        axis.title.y = element_text(family = 'STKaiti', size = 12, face = 'bold'),
        axis.text.x = element_text(family = 'STKaiti', size = 12, face = 'bold'))+
  labs(x="",y="General government revenue", main = "")
p5<-ggplot(data=country[,-1], aes(x=...8 ,y=population))+
  geom_boxplot(aes(fill=...8))+
  theme(legend.position = 'none', title = element_text(family = 'STKaiti'), # 设置不要旁边的那个解释框
        axis.title.x = element_text(family = 'STKaiti'), # 字体大小格式设置
        axis.title.y = element_text(family = 'STKaiti', size = 12, face = 'bold'),
        axis.text.x = element_text(family = 'STKaiti', size = 12, face = 'bold'))+
  labs(x="",y="population", main = "")
p6 <- ggplot(data=country[,-1], aes(x=...8 ,y=`GDP/person`))+
  geom_boxplot(aes(fill=...8))+
  theme(legend.position = 'none', title = element_text(family = 'STKaiti'), # 设置不要旁边的那个解释框
        axis.title.x = element_text(family = 'STKaiti'), # 字体大小格式设置
        axis.title.y = element_text(family = 'STKaiti', size = 12, face = 'bold'),
        axis.text.x = element_text(family = 'STKaiti', size = 12, face = 'bold'))+
  labs(x="",y="GDP/person", main = "")
grid.arrange(p1, p2,p3,p4,p5,p6 ,ncol=6)
#----层次聚类(效果较差)----
#最长距离#
data.1 <- scale(country[,-c(1,8)]) 
d <- dist(data.1) # method="man" # is a bit better
hc <- hclust(d, method = "complete")
species <- rev(levels(country$...8))
dend <- as.dendrogram(hc)
dend <- rotate(dend, 1:169)
dend <- color_branches(dend, k=4)
labels_colors(dend) <-
  rainbow_hcl(4)[sort_levels_values(
    as.numeric(country$...8)[order.dendrogram(dend)]
  )]
labels(dend) <- paste(as.character(country$...8)[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
dend <- hang.dendrogram(dend,hang_height=0.1)
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered Country", 
     horiz =  TRUE,  nodePar = list(cex = .007))
()legend("topleft", legend = species, fill = rainbow_hcl(4))#树形
par(mar = rep(0,4))
circlize_dendrogram(dend)#圆形
some_col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))
gplots::heatmap.2(as.matrix(data.1), 
                  main = "Heatmap for the Country",
                  srtCol = 20,
                  dendrogram = "row",
                  Rowv = dend,
                  Colv = "NA", # this to make sure the columns are not ordered
                  trace="none",          
                  margins =c(5,0.1),      
                  key.xlab = "Cm",
                  denscol = "grey",
                  density.info = "density",
                  RowSideColors = rev(labels_colors(dend)), # to add nice colored strips		
                  col = some_col_func
)


#----k-means------
df <- scale(country[,-c(1,8)]) 
fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
km_result <- kmeans(df, 4, nstart = 24)
dd <- cbind(df, cluster = km_result$cluster)
dd <- as.data.frame(dd)
table(dd$cluster)
table(dd$cluster,country$...8)
fwrite(table(dd$cluster,country$...8),"k-means混淆矩阵,csv")
fviz_cluster(km_result, data = dd,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = TRUE,
             ggtheme = theme_minimal()
)

#-----lda---------
country$...8 <- as.factor(country$...8 )
data.train <- country[1:135,]
data.test <- country[136:169,]
colnames(data.test) <- colnames(data.train) <- colnames(country)
fit.lda <- lda(...8~.,data.train[,-1])
fit.qda <- qda(...8~.,data.train[,-1])
fit.lda
fit.qda
p2 <- ggord(fit.lda,data.train$...8)
p2
pre <- predict(fit.lda,data.test[,-1])
pre2 <- predict(fit.qda,data.test[,-1])
table(pre$class,data.test$...8)
table(pre2$class,data.test$...8)
fwrite(table(pre$class,data.test$...8),"lda混淆矩阵,csv")
error.lda=sum(as.numeric(as.numeric(pre$class)!=as.numeric(data.test$...8)))/nrow(data.test)
print(error.lda)

error.qda=sum(as.numeric(as.numeric(pre2$class)!=as.numeric(data.test$...8)))/nrow(data.test)
print(error.lda)
error.qda
which(pre$class!=data.test$...8)
data.test[8,]
data.test[which(pre$class!=data.test$...8),]
View(data.test[which(pre$class!=data.test$...8),])
