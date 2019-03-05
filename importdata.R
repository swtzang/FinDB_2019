# Using read.table to import .txt file
etf4<-read.table("ETF4_2000_2018_d.txt")
# you will get error message! This is because Chinese characters that cannot be recognized!
# But you can add: fileEncoding = 'UTF-8-BOM' to solve the problem!
etf4<-read.table("ETF4_2000_2018_d.txt", fileEncoding = "UTF-8-BOM")
head(etf4)
# Or you can save the .txt as ansi file fromat using notepad
etf4<-read.table("ETF4_2000_2018_d_ansi.txt", header = T)
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
etf4.csv
# using read_csv to imoprt data to tibble format
install.packages("readr")
library(readr)
etf4_csv<-read_csv("ETF4_2000_2018_d.csv")
# you will get garbled text!
etf4_csv<-read_csv("ETF4_2000_2018_d.csv", locale = locale(encoding='big5'))
etf4_csv
str(etf4_csv)
#=============================================================================
# clean data
etf4.c<-etf4_csv[, c(-2, -4)]
etf4.c<-etf4.c[-1,]
colnames(etf4.c)<-c("id", "date", "price")
# use pipe operator 
library(magrittr)
#install.packages("dplyr")
library(dplyr)
etf4.c<-etf4_csv%>%select(c(1,3,5))%>%rename("id" = "證券代碼", "date"= "日期", "price" = "當日均價(元)")
#etf4.c<-etf4_csv%>%select("證券代碼", "日期", "當日均價(元)")
#-----------------------------------------------------------------                
# use dcast to reorder dataframe by date;
install.packages("reshape2")
library(reshape2)
etf4.reorder = dcast(etf4.c, date~id, na.rm=TRUE)
dim(etf4.reorder)
head(etf4.reorder)
str(etf4.reorder)
#
etf4.reorder$date<-as.Date(as.character(etf4.reorder$date), "%Y%m%d") 
head(etf4.reorder)
str(etf4.reorder)
# convert character into numeric 




