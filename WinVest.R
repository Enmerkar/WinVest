setwd("D:/Programs/RStudio/Justin")

# The method of moving averages is seriously weakened by the large 2-sided time intervals
# required for accurately modelling the curve. An alternative would be to fit a regression
# curve of order-?, calculate the differences between Close and Fit, etc.

# functions
ma <- function(x, n=3) {filter(x, rep(1/n, n), sides=2)}
encrisp <- function(x, n=5) {ifelse(filter(x, rep(1, n), sides=2)>n/2,1,ifelse(filter(x, rep(1, n), sides=2)<-n/2,-1,0))}
shed <- function(x) {
  lag <- c(NA,x[-length(x)])
  remove <- ifelse(x=lag,-1,1)*seq(1:length(x))
  x[remove]
}

# output
patterns <- data.frame('exchange.stock', 0.5, 0.5, 5, 0)
colnames(patterns) <- c('stock', 'correlation', 'volatility', 'period', 'buy')

analyseStock <- function (stock) {

  data <- read.csv(paste(stock,'.csv', sep=''), header=TRUE)
  data <- data[nrow(data):1,] # reverse order
  data <- data[1:500,] # last 2 years of data
  
  # 60-day trend
  data.ts <- ts(data[seq(nrow(data)-59, nrow(data)), 5])
  decomp <- HoltWinters(data.ts, beta=TRUE, gamma=FALSE)
  trend <- decomp$coefficients[[2]]
  
  # continue only if trend is positive
  if (trend>0) {
      
    # alternative method: fit increasing order polynomials to various periods
    
    data.3 <- cbind(seq(1:60), data$Close[seq(nrow(data)-59, nrow(data))]) # 3 months
    colnames(data.3) <- c('day','close')
    
    data.6 <- data$Close[seq(nrow(data)-120, nrow(data))] # 6 months
    data.9 <- data$Close[seq(nrow(data)-180, nrow(data))] # 9 months
    data.12 <- data$Close[seq(nrow(data)-240, nrow(data))] # 12 months
    
    fit.3.1 <- lm(close~poly(day,1), data.3)
    fit.3.2 <- lm(close~poly(day,2), data.3)
    fit.3.3 <- lm(close~poly(day,3), data.3)
    fit.3.4 <- lm(close~poly(day,4), data.3)
    
    fit.6.1 <- lm(close~poly(day,1), data.6)
    
    data.3$resid.1 <- fit.3.1$residuals
    data.3$resid.2 <- fit.3.2$residuals
    data.3$resid.3 <- fit.3.3$residuals
    data.3$resid.4 <- fit.3.4$residuals
    
    data.3$resid.1.lag <- c(NA,data.3$resid.1[-nrow(data.3)])
    
    data.3$corr.1 <- ifelse(sign(data.3$resid.1)==sign(data.3$resid.1.lag),sign(data.3$resid.1),0)
    
    # old method
    
    # moving averages
    data$ma3 <- ma(data$Close,3)
    data$ma5 <- ma(data$Close,5)
    data$ma7 <- ma(data$Close,7)
    data$ma9 <- ma(data$Close,9)
    data$ma11 <- ma(data$Close,11)
    data$ma13 <- ma(data$Close,13)
    data$ma15 <- ma(data$Close,15)
    data$ma17 <- ma(data$Close,17)
    data$ma19 <- ma(data$Close,19)
    data$ma21 <- ma(data$Close,21)
    
    data$ma35 <- ma(data$Close,35)
    data$ma55 <- ma(data$Close,55)
    data$ma101 <- ma(data$Close,101)
    
    # close-to-average differences
    data$diff3 <- data$Close-data$ma3
    data$diff5 <- data$Close-data$ma5
    data$diff7 <- data$Close-data$ma7
    data$diff9 <- data$Close-data$ma9
    data$diff11 <- data$Close-data$ma11
    data$diff13 <- data$Close-data$ma13
    
    # lag forward differences
    data$lag3 <- c(NA,data$diff3[-nrow(data)])
    data$lag5 <- c(NA,data$diff5[-nrow(data)])
    data$lag7 <- c(NA,data$diff7[-nrow(data)])
    data$lag9 <- c(NA,data$diff9[-nrow(data)])
    data$lag11 <- c(NA,data$diff11[-nrow(data)])
    data$lag13 <- c(NA,data$diff13[-nrow(data)])
    
    #data <- na.omit(data)
    
    # day-to-day correlations
    data$corr3 <- ifelse(sign(data$diff3)==sign(data$lag3),sign(data$diff3),0)
    data$corr5 <- ifelse(sign(data$diff5)==sign(data$lag5),sign(data$diff3),0)
    data$corr7 <- ifelse(sign(data$diff7)==sign(data$lag7),sign(data$diff3),0)
    data$corr9 <- ifelse(sign(data$diff9)==sign(data$lag9),sign(data$diff3),0)
    data$corr11 <- ifelse(sign(data$diff11)==sign(data$lag11),sign(data$diff11),0)
    data$corr13 <- ifelse(sign(data$diff13)==sign(data$lag13),sign(data$diff13),0)
    
    # now need to look at various intervals, 3,6,9,12 months, for each ma
    
    # repeatedly apply filter to corr's until they are unchanged
    pat3a <- data$corr3
    pat3b <- encrisp(pat3a)
    while (pat3b != pat3a) {
      pat3a <- pat3b
      pat3b <- encrisp(pat3a)
    }
    
    # calculate stability: average length of 1 and -1 regions compared to whole period
    # we don't want patterns which change too often
    
    # remove repeated entries in corr until only the pattern of 1,0,-1 remains
    pat3b <- shed(pat3a)
    while (pat3b != pat3a) {
      pat3a <- pat3b
      pat3b <- shed(pat3a)
    }
    
    # choose only patterns which have a minimum number of appropriate oscillations
    # e.g. at least 2 swings but no more than 10.
    
    
    
    # Correlation: fewer 0's is more correlated. Set maximum of say 10% 0's.
    # Calculate this from raw, not filtered, correlation data
    
    # Volatility: alternating regions of 1's and -1's. Calculated from shedded data.
    
    # Buy Index: measure of closeness to minimum (1 to -1) of cycle.
    
    data
    
    # return good patterns
    #if(corr3>0.5 && vol3>0.5) patterns <- rbind(patterns, c(stock, corr3, vol3, per3, buy))
    #if(corr5>0.5 && vol5>0.5) patterns <- rbind(patterns, c(stock, corr5, vol5, per5, buy))

  }
  
  # clean environment
  #rm(data)
  
}

asx.slx <- analyseStock('SLX')
