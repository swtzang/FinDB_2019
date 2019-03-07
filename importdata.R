# Fin_db_mgmt_2019
# Reference:
# 1. https://rpubs.com/mohammadshadan/288218
# 2. MANIPULATING TIME SERIES DATA IN R WITH XTS & ZOO, datacamp course slide.
#-----------------------------------------------------------------------------
# Using read.table to import .txt file
etf4<-read.table("ETF4_2000_2018_d.txt")
# you will get error message! This is because Chinese characters that cannot be recognized!
# But this works fine in Mac!!!
# But you can add: fileEncoding = 'UTF-8-BOM' to solve the problem!
etf4<-read.table("ETF4_2000_2018_d.txt", fileEncoding = "UTF-8-BOM")
head(etf4)
# Or you can save the .txt as ansi file fromat using notepad
etf4<-read.table("ETF4_2000_2018_d_ansi.txt", header = T)
# You will get error message in Mac
str(etf4)
# change imoprted data types
etf4<-read.table("ETF4_2000_2018_d_ansi.txt", header = T, stringsAsFactors = T, 
                 colClasses = c("證券代碼"="character"))
str(etf4)
head(etf4)
# Using read.csv to import .csv file
etf4<-read.csv("ETF4_2000_2018_d.csv", colClasses = c("證券代碼"="character"))
str(etf4)
# If you get garbled text, you may try:
etf4.csv<-read.csv("ETF4_2000_2018_d.csv", fileEncoding='big5', 
                   colClasses=c('factor', 'factor', 'factor', 'numeric', 'numeric'))
head(etf4.csv)
str(etf4.csv)
# using read_csv to imoprt data to tibble format
install.packages("readr")
library(readr)
etf4_csv<-read_csv("ETF4_2000_2018_d.csv")
# you will get garbled text!
etf4_csv<-read_csv("ETF4_2000_2018_d.csv", locale = locale(encoding='big5'))
head(etf4_csv)
str(etf4_csv)
# read xls file
install.packages("readxl")
library(readxl)
etf4_xls<-read_excel("ETF4_2000_2018_d.xls", 
                     col_types =c("text", "text","text", "numeric","numeric"))
head(etf4_xls)
#=============================================================================
# clean data
etf4.c<-etf4_csv[, c(-2, -4)]
etf4.c<-etf4.c[-1,]
colnames(etf4.c)<-c("id", "date", "price")
# use pipe operator 
library(magrittr)
install.packages("dplyr")
library(dplyr)
etf4.c<-etf4_csv%>%select(c(1,3,5))%>%rename("id" = "證券代碼", "date"= "日期", "price" = "當日均價(元)")
etf4.c
#etf4.c<-etf4_csv%>%select("證券代碼", "日期", "當日均價(元)")
#-----------------------------------------------------------------                
# use dcast to reorder dataframe by date;
install.packages("reshape2")
library(reshape2)
etf4.reorder = dcast(etf4.c, date~id, na.rm=TRUE)
dim(etf4.reorder)
head(etf4.reorder)
str(etf4.reorder)
# convert into date format using as.Date()
etf4.reorder$date<-as.Date(as.character(etf4.reorder$date), "%Y%m%d") 
head(etf4.reorder)
str(etf4.reorder)
# convert character into numeric 
# convert to xts
install.packages("xts")
library(xts)
etf4.xts<-xts(etf4.reorder[,-1], order.by = etf4.reorder$date)
head(etf4.xts)
tail(etf4.xts)
str(etf4.xts)
# delete NA values
etf4.xts<-na.omit(etf4.xts)
head(etf4.xts)
# or complete cases
install.packages("tidyr")
library(tidyr)
etf4.xts1<-etf4.xts[complete.cases(etf4.xts),]
head(etf4.xts1)
#-----------------------------------------------------------
# export data
#----------------------------------------------------------
write.csv(etf4.xts, file = "myetf4.csv")
# date index disappears!!!
write.zoo(etf4.xts, sep = ',', file = "myetf4.csv.1")
saveRDS(etf4.xts, file = "etf4.xts.rds")
etf4.xts2 <- readRDS("etf4.xts.rds")
head(etf4.xts2)
#------------------------------------------------------------
# Converting Daily Prices to Monthly Returns in the xts world
#------------------------------------------------------------
install.packages('quantmod')
library(quantmod)
etf4_monthly <- to.monthly(etf4.xts, indexAt = "lastof", OHLC=FALSE)
head(etf4_monthly)
# convert daily prices to weekly returns
etf4_weekly<-to.weekly(etf4.xts, indexAt = "lastof", OHLE = FALSE)
head(etf4_weekly)
dim(etf4_weekly)
# however, you will lose column names
# you can use the following to keep column names
etf4_weekly <- etf4.xts[endpoints(etf4.xts, on="weeks", k=1), ]
head(etf4_weekly)
dim(etf4_weekly)
#
install.packages('PerformanceAnalytics', 'magrittr')
library(PerformanceAnalytics)
library(magrittr)
etf4_returns_xts <-Return.calculate(etf4_monthly, method = "log") %>%
  na.omit()
head(etf4_returns_xts)
dim(etf4_returns_xts)
#================================================
# some manipulations on xts 
#================================================
dates <- as.Date("2016-01-01") + 0:40
# Create ts_a
ts_a <- xts(x = 1:41, order.by = dates)
ts_a
# Create ts_b
ts_b <- xts(x = 1:41, order.by = as.POSIXct(dates))
ts_b
ts_a[index(ts_a)]
ts_a[index(ts_b)]
#
tmp_file <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1127/datasets/tmp_file.csv"
# Create dat by reading tmp_file
dat <- read.csv(tmp_file)
head(dat)
str(dat)
# Convert dat into xts
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))

# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")
dat_zoo
str(dat_zoo)
# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)
#----------------------------------------------