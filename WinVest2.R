setwd("~/WinVest")

# cleans neighbouring signs
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

# output
patterns <- data.frame('exchange.stock', 0.5, 0.5, 5, 0)
colnames(patterns) <- c('stock', 'correlation', 'volatility', 'period', 'buy')

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
  
  # Get max/min extrema according to correlations
  data.3$extrema.1 <- ifelse(data.3$crisp.1==1,data.3$high-data.3$model.1,ifelse(data.3$crisp.1==(-1),data.3$low-data.3$model.1,0))
  data.3$extrema.2 <- ifelse(data.3$crisp.2==1,data.3$high-data.3$model.2,ifelse(data.3$crisp.2==(-1),data.3$low-data.3$model.2,0))
  data.3$extrema.3 <- ifelse(data.3$crisp.3==1,data.3$high-data.3$model.3,ifelse(data.3$crisp.3==(-1),data.3$low-data.3$model.3,0))
  data.3$extrema.4 <- ifelse(data.3$crisp.4==1,data.3$high-data.3$model.4,ifelse(data.3$crisp.4==(-1),data.3$low-data.3$model.4,0))
  
  # remove repeated entries in corr until only the pattern of 1,0,-1 remains
  pattern.3.1 <- run_shed(crisp.3.1)
  
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
    
  
  # clean environment
  #rm(data)
  
}

asx.slx <- analyseStock('SLX')
