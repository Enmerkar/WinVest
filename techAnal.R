library("TTR")
library("graphics")

runSLX <- function () {
  data <- read.csv('SLX.csv')
  a<-techPrepTS(data)
  #data2 <- techPrep(data)
  #data2[1:10,]
}

techPrepTS <- function (data) {
  timedata <- ts(data[2:7],start=1,end=nrow(data),frequency=1)
  pacf(timedata[,4])
  adf.test(timedata[,4], alternative="stationary")
}

techPrep <- function (data) {
  
  data$DayMid <- (data$High+data$Low)/2
  
  ma <- function(x,n=3){filter(x,rep(1/n,n), sides=2)}
  data$MA3 <- ma(data$DayMid,3)
  data$MA5 <- ma(data$DayMid,5)
  data$MA7 <- ma(data$DayMid,7)
  data$MA9 <- ma(data$DayMid,9)
  
  data$Diff3 <- data$Close-data$MA3
  data$Diff5 <- data$Close-data$MA5
  data$Diff7 <- data$Close-data$MA7
  data$Diff9 <- data$Close-data$MA9
  
  data$test <- c(data$Diff3[-1],NA)
  data$test2 <- c(NA,data$Diff3[-1])
  
  data$Corr3 <- (data$Diff3 - c(data$Diff3[-1],NA))/data$Diff3
  
  #   data$Corr3 <- (data$Diff3 - data$Diff3[-1])/data$Diff3
  #   data$Corr5 <- (data$Diff5 - data$Diff5[-1])/data$Diff5
  #   data$Corr7 <- (data$Diff7 - data$Diff7[-1])/data$Diff7
  #   data$Corr9 <- (data$Diff9 - data$Diff9[-1])/data$Diff9
  #   
  #   data$AdjCorr3 <- if(abs(data$Corr3)<1) {1/abs(data$Corr3)} else {abs(data$Corr3)}
  
  data
}

techAnal <- function (data) {
  
  #Get 1,2,3,4,5,6 month MA regression model slope and goodness of fit
  
}