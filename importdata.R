etf4<-read.table("ETF4_2000_2018_d.txt")

etf4<-etf4[, c(-2, -4)]
etf4<-etf4[-1,]
colnames(etf4)<-c("id", "date", "price")