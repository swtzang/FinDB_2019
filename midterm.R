#================================
# Midterm exam:
#

rm(list=ls())

tw20.code<-read.csv("2018Q4_20.csv", fileEncoding='big5', colClasses = c("name"="character"))
str(tw20.code)
tw20.code$name
#
tw20.code.yahoo<-sapply(tw20.code$name, )

#
library(readr)
tw20_code<-read_csv("2018Q4_20.csv", locale = locale(encoding='big5'))
tw20_code
str(tw20_code)
#
tw20<-read.table("tw20.txt", fileEncoding = "UTF-8-BOM", header = T)
head(tw20)
tw20<-tw20[,-2]
colnames(tw20)<-c("code", "date", "price")
head(tw20)
#
library(reshape2)
library(xts)
tw20.reorder = dcast(tw20, date~code)
head(tw20.reorder)
dim(tw20.reorder)
tw20.xts<-xts(tw20.reorder[,-1], order.by = as.Date(as.character(tw20.reorder[,1]), format = "%Y%m%d"))
head(tw20.xts)
class(tw20.xts)
#
library(quantmod)
tw20_mon <- to.monthly(tw20.xts, indexAt = "lastof", OHLC=FALSE)
head(tw20_mon)
#
#install.packages('PerformanceAnalytics', 'magrittr')
library(PerformanceAnalytics)
#library(magrittr)
tw20_mon_ret <-Return.calculate(tw20_mon, method = "log")
head(tw20_mon_ret)
dim(tw20_mon_ret)
#
tw20_day_ret <-Return.calculate(tw20.xts, method = "log")
head(tw20_day_ret)
dim(tw20_day_ret)
#

