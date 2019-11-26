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
