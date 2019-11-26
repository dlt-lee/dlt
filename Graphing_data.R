library(Rcmdr)
library(gmodels)
library(XLConnect)
library(car)
library(grid)
library(lattice)
library(epcalc)
library(ggplot2)
source("element.R")

#lattice
xyplot(dlt$sa~dlt$sb|dlt$a1,pch = 16)
#ggplot2
ggplot(dlt,aes(x=a1,y=b1)) + geom_point()
#带状图
stripChart(dlt[,4:10])
#抖动带状图
stripchart(dlt$a5,jitter = 0.1, pch = 16)
#去掉框
par(bty="n")
#xlim/ylim x、y拉伸
stripchart(dlt$sa~dlt$sb,jitter = 0.1,pch = 1,xlim = c(20,160),ylim = c(0,23),xlab = "a1")
#坐标轴控制
axis(1,col="dodgerblue4",at = c(20,40,60,80,100,120,140,160))
#添加注释
mtext("大乐透历史数据",side = 1,#下方
      line = 4,#下方
      adj= 0,#左对齐
      col = "dodgerblue4",#颜色
      cex =2)#字体大小
