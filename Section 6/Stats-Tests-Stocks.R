#Note: The calculations in this script take only a couple of seconds on an Intel(R) Xeon(R)
#      Gold 6136 CPU with 3.00 GHz on a 64-Bit Windows Server 2016 with 24 processors.

library("quantmod")
library("tseries")
library("timeDate")

#load DJ data
DJ = getSymbols(Symbols = "^DJI", src = "yahoo", from = "1986-01-01", 
                to = "2020-01-01", auto.assign = F)
DJ = DJ$DJI.Adjusted
rDJ = diff(log(as.vector(DJ)))

#load S&P500 data
SP500 = getSymbols(Symbols = "^GSPC", src = "yahoo", from = "1986-01-01", 
                   to = "2020-01-01", auto.assign = F)
SP500 = SP500$GSPC.Adjusted
rSP500 = diff(log(as.vector(SP500)))

#statistics and tests
TestPipe = function(vec){
  vec = as.vector(vec)
  
  #Jarque-Bera test: H_0: data is normal distributed
  JB = jarque.bera.test(vec)$p.value
  
  #Ljung-Box test: H_0: no serial correlation
  LB = Box.test(vec, lag = 8, type = c("Ljung-Box"))$p.value
  
  #ADF-test: H_0: ts is non-stationary
  ADF = adf.test(vec, k = 8)$statistic
  
  #PP-test: H_0: ts is non-stationary
  PP = PP.test(vec)$statistic
  
  #KPSS-test: H_0: ts is (level) stationary
  KPSS = kpss.test(vec)$statistic
  
  return(round(c("Min" = min(vec),
                 "Mean" = mean(vec),
                 "Median" = median(vec),
                 "Max" = max(vec),
                 "Sd" = sd(vec),
                 "Kurtosis" = kurtosis(vec),
                 "Skewness" = skewness(vec),
                 "JB" = JB,
                 "LB" = LB,
                 "ADF" = ADF,
                 "PP" = PP,
                 "KPSS" = KPSS),4))
}

res = cbind(TestPipe(rDJ), TestPipe(rSP500))
colnames(res) = c("DJ", "S&P500")
