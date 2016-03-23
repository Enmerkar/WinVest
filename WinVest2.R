setwd("~/WinVest")

# cleans neighbouring signs - experiment with n=3 and n=5
encrisp <- function(x, n=5) {sign(filter(x, rep(1, n), sides=2))}
run_encrisp <- function(corr) {
  pattern.a <- corr
  pattern.a <- ifelse(is.na(pattern.a),0,pattern.a)
  pattern.b <- encrisp(pattern.a)
  pattern.b <- ifelse(is.na(pattern.b),0,pattern.b)
  while (any(pattern.a != pattern.b)) {
    pattern.a <- pattern.b
    pattern.b <- encrisp(pattern.a)
    pattern.b <- ifelse(is.na(pattern.b),0,pattern.b)
  }
  lag <- c(pattern.a[-1],NA)
  pattern.a <- ifelse(pattern.a==0,lag,pattern.a)
  pattern.a[length(pattern.a)]=0
  pattern.a
}

# removes correlated signs
shed <- function(x) {
  lag <- c(NA,x[-length(x)])
  remove <- ifelse(x==lag,FALSE,TRUE)*seq(1:length(x))
  x[remove]
}
run_shed <- function(crisp) {
  pattern.a <- crisp
  pattern.b <- shed(pattern.a)
  while(length(pattern.a) != length(pattern.b)) {
    pattern.a <- pattern.b
    pattern.b <- shed(pattern.a)
  }
  pattern.a[2:(length(pattern.a)-1)]
}

# creates factor labels for correlated regions
set_regions <- function (crisp) {
  l <- length(crisp)
  m <- 1
  n <- 1
  lag <- c(NA,crisp[-l])
  region <- vector(length=l)
  while (n <= l) {
    if (n>1 && crisp[n]==(-1*lag[n])) {m <- m+1}
    if (abs(crisp[n]) != 1) {
      region[n] = 0
    } else {
      region[n] = m
    }
    n <- n+1
  }
  as.factor(region)
}

# output
patterns <- data.frame('exchange.stock', date(), 0.5, 5, 0.5, 5, 0)
colnames(patterns) <- c('stock', 'date', 'correlation', 'swings', 'amplitude', 'period', 'buy')

# Eventually write this function with polynomial and period arguments
# Then call iteratively from another function with a polynomial limit argument
# and a vector of periods argument
analyseStock <- function (stock) {
  
  stock='qan'
  data <- read.csv(paste(stock,'.csv', sep=''), header=TRUE)
  data <- data[nrow(data):1,] # reverse order
  
  data.3 <- data.frame(seq(1:60), data$Close[seq(nrow(data)-59, nrow(data))], data$High[seq(nrow(data)-59, nrow(data))], data$Low[seq(nrow(data)-59, nrow(data))]) # 3 months
  data.6 <- data.frame(seq(1:120), data$Close[seq(nrow(data)-119, nrow(data))], data$High[seq(nrow(data)-119, nrow(data))], data$Low[seq(nrow(data)-119, nrow(data))]) # 6 months
  data.9 <- data.frame(seq(1:180), data$Close[seq(nrow(data)-179, nrow(data))], data$High[seq(nrow(data)-179, nrow(data))], data$Low[seq(nrow(data)-179, nrow(data))]) # 9 months
  data.12 <- data.frame(seq(1:240), data$Close[seq(nrow(data)-239, nrow(data))], data$High[seq(nrow(data)-239, nrow(data))], data$Low[seq(nrow(data)-239, nrow(data))]) # 12 months
  
  colnames(data.3) <- c('day','close','high','low')
  colnames(data.6) <- c('day','close','high','low')
  colnames(data.9) <- c('day','close','high','low')
  colnames(data.12) <- c('day','close','high','low')
  
  # Run up to 4th order polynomial fits
  fit.3.1 <- lm(close~poly(day,1,raw=TRUE), data.3)
  fit.3.2 <- lm(close~poly(day,2,raw=TRUE), data.3)
  fit.3.3 <- lm(close~poly(day,3,raw=TRUE), data.3)
  fit.3.4 <- lm(close~poly(day,4,raw=TRUE), data.3)
  
  fit.6.1 <- lm(close~poly(day,1,raw=TRUE), data.6)
  fit.6.2 <- lm(close~poly(day,2,raw=TRUE), data.6)
  fit.6.3 <- lm(close~poly(day,3,raw=TRUE), data.6)
  fit.6.4 <- lm(close~poly(day,4,raw=TRUE), data.6)
  
  fit.9.1 <- lm(close~poly(day,1,raw=TRUE), data.9)
  fit.9.2 <- lm(close~poly(day,2,raw=TRUE), data.9)
  fit.9.3 <- lm(close~poly(day,3,raw=TRUE), data.9)
  fit.9.4 <- lm(close~poly(day,4,raw=TRUE), data.9)
  
  fit.12.1 <- lm(close~poly(day,1,raw=TRUE), data.12)
  fit.12.2 <- lm(close~poly(day,2,raw=TRUE), data.12)
  fit.12.3 <- lm(close~poly(day,3,raw=TRUE), data.12)
  fit.12.4 <- lm(close~poly(day,4,raw=TRUE), data.12)
  
  # Get the predicted from the models
  data.3$model.1 <- fit.3.1$fitted.values
  data.3$model.2 <- fit.3.2$fitted.values
  data.3$model.3 <- fit.3.3$fitted.values
  data.3$model.4 <- fit.3.4$fitted.values
  
  data.6$model.1 <- fit.6.1$fitted.values
  data.6$model.2 <- fit.6.2$fitted.values
  data.6$model.3 <- fit.6.3$fitted.values
  data.6$model.4 <- fit.6.4$fitted.values
  
  data.9$model.1 <- fit.9.1$fitted.values
  data.9$model.2 <- fit.9.2$fitted.values
  data.9$model.3 <- fit.9.3$fitted.values
  data.9$model.4 <- fit.9.4$fitted.values
  
  data.12$model.1 <- fit.12.1$fitted.values
  data.12$model.2 <- fit.12.2$fitted.values
  data.12$model.3 <- fit.12.3$fitted.values
  data.12$model.4 <- fit.12.4$fitted.values
  
  # Get the residuals from the models
  data.3$resid.1 <- fit.3.1$residuals
  data.3$resid.2 <- fit.3.2$residuals
  data.3$resid.3 <- fit.3.3$residuals
  data.3$resid.4 <- fit.3.4$residuals
  
  data.6$resid.1 <- fit.6.1$residuals
  data.6$resid.2 <- fit.6.2$residuals
  data.6$resid.3 <- fit.6.3$residuals
  data.6$resid.4 <- fit.6.4$residuals
  
  data.9$resid.1 <- fit.9.1$residuals
  data.9$resid.2 <- fit.9.2$residuals
  data.9$resid.3 <- fit.9.3$residuals
  data.9$resid.4 <- fit.9.4$residuals
  
  data.12$resid.1 <- fit.12.1$residuals
  data.12$resid.2 <- fit.12.2$residuals
  data.12$resid.3 <- fit.12.3$residuals
  data.12$resid.4 <- fit.12.4$residuals
  
  # Lag the residuals
  data.3$resid.1.lag <- c(NA,data.3$resid.1[-nrow(data.3)])
  data.3$resid.2.lag <- c(NA,data.3$resid.2[-nrow(data.3)])
  data.3$resid.3.lag <- c(NA,data.3$resid.3[-nrow(data.3)])
  data.3$resid.4.lag <- c(NA,data.3$resid.4[-nrow(data.3)])
  
  data.6$resid.1.lag <- c(NA,data.6$resid.1[-nrow(data.6)])
  data.6$resid.2.lag <- c(NA,data.6$resid.2[-nrow(data.6)])
  data.6$resid.3.lag <- c(NA,data.6$resid.3[-nrow(data.6)])
  data.6$resid.4.lag <- c(NA,data.6$resid.4[-nrow(data.6)])
  
  data.9$resid.1.lag <- c(NA,data.9$resid.1[-nrow(data.9)])
  data.9$resid.2.lag <- c(NA,data.9$resid.2[-nrow(data.9)])
  data.9$resid.3.lag <- c(NA,data.9$resid.3[-nrow(data.9)])
  data.9$resid.4.lag <- c(NA,data.9$resid.4[-nrow(data.9)])
  
  data.12$resid.1.lag <- c(NA,data.12$resid.1[-nrow(data.12)])
  data.12$resid.2.lag <- c(NA,data.12$resid.2[-nrow(data.12)])
  data.12$resid.3.lag <- c(NA,data.12$resid.3[-nrow(data.12)])
  data.12$resid.4.lag <- c(NA,data.12$resid.4[-nrow(data.12)])
  
  # Calculate daily correlations
  data.3$corr.1 <- ifelse(sign(data.3$resid.1)==sign(data.3$resid.1.lag),sign(data.3$resid.1),0)
  data.3$corr.2 <- ifelse(sign(data.3$resid.2)==sign(data.3$resid.2.lag),sign(data.3$resid.2),0)
  data.3$corr.3 <- ifelse(sign(data.3$resid.3)==sign(data.3$resid.3.lag),sign(data.3$resid.3),0)
  data.3$corr.4 <- ifelse(sign(data.3$resid.4)==sign(data.3$resid.4.lag),sign(data.3$resid.4),0)
  
  data.6$corr.1 <- ifelse(sign(data.6$resid.1)==sign(data.6$resid.1.lag),sign(data.6$resid.1),0)
  data.6$corr.2 <- ifelse(sign(data.6$resid.2)==sign(data.6$resid.2.lag),sign(data.6$resid.2),0)
  data.6$corr.3 <- ifelse(sign(data.6$resid.3)==sign(data.6$resid.3.lag),sign(data.6$resid.3),0)
  data.6$corr.4 <- ifelse(sign(data.6$resid.4)==sign(data.6$resid.4.lag),sign(data.6$resid.4),0)
  
  data.9$corr.1 <- ifelse(sign(data.9$resid.1)==sign(data.9$resid.1.lag),sign(data.9$resid.1),0)
  data.9$corr.2 <- ifelse(sign(data.9$resid.2)==sign(data.9$resid.2.lag),sign(data.9$resid.2),0)
  data.9$corr.3 <- ifelse(sign(data.9$resid.3)==sign(data.9$resid.3.lag),sign(data.9$resid.3),0)
  data.9$corr.4 <- ifelse(sign(data.9$resid.4)==sign(data.9$resid.4.lag),sign(data.9$resid.4),0)
  
  data.12$corr.1 <- ifelse(sign(data.12$resid.1)==sign(data.12$resid.1.lag),sign(data.12$resid.1),0)
  data.12$corr.2 <- ifelse(sign(data.12$resid.2)==sign(data.12$resid.2.lag),sign(data.12$resid.2),0)
  data.12$corr.3 <- ifelse(sign(data.12$resid.3)==sign(data.12$resid.3.lag),sign(data.12$resid.3),0)
  data.12$corr.4 <- ifelse(sign(data.12$resid.4)==sign(data.12$resid.4.lag),sign(data.12$resid.4),0)
  
  # Overall correlation coefficents
  corr.3.1 <- sum(abs(data.3$corr.1), na.rm=TRUE)/(nrow(data.3)-1)
  corr.3.2 <- sum(abs(data.3$corr.2), na.rm=TRUE)/(nrow(data.3)-1)
  corr.3.3 <- sum(abs(data.3$corr.3), na.rm=TRUE)/(nrow(data.3)-1)
  corr.3.4 <- sum(abs(data.3$corr.4), na.rm=TRUE)/(nrow(data.3)-1)
  
  # Clean correlations until they are smooth and stable
  data.3$crisp.1 <- run_encrisp(data.3$corr.1)
  data.3$crisp.2 <- run_encrisp(data.3$corr.2)
  data.3$crisp.3 <- run_encrisp(data.3$corr.3)
  data.3$crisp.4 <- run_encrisp(data.3$corr.4)
  
  data.6$crisp.1 <- run_encrisp(data.6$corr.1)
  data.6$crisp.2 <- run_encrisp(data.6$corr.2)
  data.6$crisp.3 <- run_encrisp(data.6$corr.3)
  data.6$crisp.4 <- run_encrisp(data.6$corr.4)
  
  data.9$crisp.1 <- run_encrisp(data.9$corr.1)
  data.9$crisp.2 <- run_encrisp(data.9$corr.2)
  data.9$crisp.3 <- run_encrisp(data.9$corr.3)
  data.9$crisp.4 <- run_encrisp(data.9$corr.4)
  
  data.12$crisp.1 <- run_encrisp(data.12$corr.1)
  data.12$crisp.2 <- run_encrisp(data.12$corr.2)
  data.12$crisp.3 <- run_encrisp(data.12$corr.3)
  data.12$crisp.4 <- run_encrisp(data.12$corr.4)
  
  # Need to give each region a factor label
  data.3$region.1 <- set_regions(data.3$crisp.1)
  data.3$region.2 <- set_regions(data.3$crisp.2)
  data.3$region.3 <- set_regions(data.3$crisp.3)
  data.3$region.4 <- set_regions(data.3$crisp.4)
  
  # average period length of regions
  period.3.1 <- mean(as.vector(table(data.3$region.1))[-1])
  period.3.2 <- mean(as.vector(table(data.3$region.2))[-1])
  period.3.3 <- mean(as.vector(table(data.3$region.3))[-1])
  period.3.4 <- mean(as.vector(table(data.3$region.4))[-1])
  
  # Get max/min extrema according to correlations
  data.3$extrema.1 <- ifelse(data.3$crisp.1==1,data.3$high-data.3$model.1,ifelse(data.3$crisp.1==(-1),data.3$low-data.3$model.1,0))
  data.3$extrema.2 <- ifelse(data.3$crisp.2==1,data.3$high-data.3$model.2,ifelse(data.3$crisp.2==(-1),data.3$low-data.3$model.2,0))
  data.3$extrema.3 <- ifelse(data.3$crisp.3==1,data.3$high-data.3$model.3,ifelse(data.3$crisp.3==(-1),data.3$low-data.3$model.3,0))
  data.3$extrema.4 <- ifelse(data.3$crisp.4==1,data.3$high-data.3$model.4,ifelse(data.3$crisp.4==(-1),data.3$low-data.3$model.4,0))
  
  # remove repeated entries in corr until only the pattern of 1,0,-1 remains
  pattern.3.1 <- run_shed(data.3$crisp.1)
  pattern.3.2 <- run_shed(data.3$crisp.2)
  pattern.3.3 <- run_shed(data.3$crisp.3)
  pattern.3.4 <- run_shed(data.3$crisp.4)
  
  # count swings
  swings.3.1 <- length(pattern.3.1)
  swings.3.2 <- length(pattern.3.2)
  swings.3.3 <- length(pattern.3.3)
  swings.3.4 <- length(pattern.3.4)
  
  # replicate region factors for patterns
  region.3.1 <- as.factor(seq(1:length(pattern.3.1)))
  region.3.2 <- as.factor(seq(1:length(pattern.3.2)))
  region.3.3 <- as.factor(seq(1:length(pattern.3.3)))
  region.3.4 <- as.factor(seq(1:length(pattern.3.4)))
  
  # Then extract max/min extrema from each region
  extrema.3.1 <- as.vector(aggregate(abs(data.3$extrema.1), by=list(data.3$region.1), FUN=max))[-1,2]*pattern.3.1
  extrema.3.2 <- as.vector(aggregate(abs(data.3$extrema.2), by=list(data.3$region.2), FUN=max))[-1,2]*pattern.3.2
  extrema.3.3 <- as.vector(aggregate(abs(data.3$extrema.3), by=list(data.3$region.3), FUN=max))[-1,2]*pattern.3.3
  extrema.3.4 <- as.vector(aggregate(abs(data.3$extrema.4), by=list(data.3$region.4), FUN=max))[-1,2]*pattern.3.4
  
  # Overall amplitude coefficients
  amplitude.3.1 <- mean(abs(diff(extrema.3.1, lag=1, differences=1)))
  amplitude.3.2 <- mean(abs(diff(extrema.3.2, lag=1, differences=1)))
  amplitude.3.3 <- mean(abs(diff(extrema.3.3, lag=1, differences=1)))
  amplitude.3.4 <- mean(abs(diff(extrema.3.4, lag=1, differences=1)))
  
  # Buy Index: measure of closeness to minimum of cycle.
  # 1 means current price is at trough of cycle - ideal time to buy
  # -1 means current price is at peak of cycle - ideal time to sell
  buy.3.1 <- data.3$resid.1[length(data.3$resid.1)]/(amplitude.3.1/-2)
  buy.3.2 <- data.3$resid.1[length(data.3$resid.2)]/(amplitude.3.2/-2)
  buy.3.3 <- data.3$resid.1[length(data.3$resid.3)]/(amplitude.3.3/-2)
  buy.3.4 <- data.3$resid.1[length(data.3$resid.4)]/(amplitude.3.4/-2)
  
  # choose only patterns which have a minimum number of appropriate oscillations
  # e.g. at least 2 swings but no more than 10.
  
  # Return aggregate information
  patterns <- rbind(patterns, c(stock, date(), corr.3.1, swings.3.1, amplitude.3.1, period.3.1, buy.3.1))
  
  # clean environment
  rm(data)
  
}

asx.slx <- analyseStock('SLX')
