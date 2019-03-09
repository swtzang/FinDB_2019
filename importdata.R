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
#-----------------------------------------------
#install.packages("readr")
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
#install.packages("reshape2")
library(reshape2)
etf4.reorder = dcast(etf4.c, date~id, na.rm=TRUE)
dim(etf4.reorder)
head(etf4.reorder)
str(etf4.reorder)
# convert date series into date format
etf4.reorder$date<-as.Date(as.character(etf4.reorder$date), "%Y%m%d") 
head(etf4.reorder)
str(etf4.reorder)

# convert to xts
library(xts)
etf4.xts<-xts(etf4.reorder[,-1], order.by = etf4.reorder$date)
head(etf4.xts)
tail(etf4.xts)
str(etf4.xts)
#------------------------------------------------------------
# remove NA rows
etf4.xts<-na.omit(etf4.xts)
head(etf4.xts)
# or complete cases
install.packages("tidyr")
library(tidyr)
etf4.xts1<-etf4.xts[complete.cases(etf4.xts),]
head(etf4.xts1)
#----------------------------------------------------------
# Converting Daily Prices to Monthly Returns in the xts world
etf4_day <- coredata(etf4.xts[-1,])/ coredata(etf4.xts[-dim(etf4.xts)[1],]) - 1
head(etf4_day)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
etf4_day_ret<-Return.calculate(etf4.xts)
head(etf4_day_ret)
#----------------------------------------------------------
etf4_monthly <- to.monthly(etf4.xts, indexAt = "lastof", OHLC =FALSE)
head(etf4_monthly)
# Try to convert data into weekly returns

library(magrittr)
etf4_mon_ret <-Return.calculate(etf4_monthly, method = "log") %>%
  na.omit()
head(etf4_mon_ret)
dim(etf4_mon_ret)
#-------------------------------------------------------------
plot(etf4_mon_ret)
plot.xts(etf4_mon_ret, auto.legend = TRUE)
#-----------------------------------------------------------
library(tidyverse)
library(ggplot2)
# convert xts into data frame which can be used by ggplot
#etf4_returns.df<-fortify(etf4_returns_xts)
etf4_returns.df<-fortify(etf4_returns_xts, melt=TRUE)
head(etf4_returns.df)
#
p<-ggplot(etf4_returns.df, aes(x = Index, y = Value))+
  geom_line(aes(color = Series), size = 1)

p + scale_x_date(date_labels = "%b/%Y")

# histogram distribution
q<-etf4_returns.df %>%
  ggplot(aes(x =Value, fill = Series)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Monthly Returns")
q + facet_wrap(~Series)+ theme_update(plot.title = element_text(hjust = 0.5))


# line distribution
etf4_returns.df %>%
  ggplot(aes(x = Value, colour = Series)) +
  geom_density(alpha = 1) +
  ggtitle("Monthly Returns Density Since 2016") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

# Combine line and histogram together
etf4_returns.df %>%
  ggplot(aes(x = Value)) +
  geom_density(aes(color = Series), alpha = 1) +
  geom_histogram(aes(fill = Series), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~Series) +
  ggtitle("Monthly Returns Since 2016") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

# plot the scatterplot of 0050 and 00646
# convert xts into df 
etf4_returns.df1<-fortify(etf4_returns_xts)
plot(etf4_returns.df1$`0050`, etf4_returns.df1$`00646`, pch=20,
     col = 'darkred', main = '0050 vs. 00646 monthly returns',
     xlab = '0050', ylab = '00646 S&P500')




