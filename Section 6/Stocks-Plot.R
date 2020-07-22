#Note: The calculations in this script take only a couple of seconds on an Intel(R) Xeon(R)
#      Gold 6136 CPU with 3.00 GHz on a 64-Bit Windows Server 2016 with 24 processors.

library("quantmod")
library("zoo")
library("lubridate")

#load DJ data
DJ = getSymbols(Symbols = "^DJI", src = "yahoo", from = "1986-01-01", 
                to = "2020-01-01", auto.assign = F)
DJ = DJ$DJI.Adjusted
rDJ = zoo(diff(log(as.vector(DJ))), time(DJ)[-1])

#load S&P500 data
SP500 = getSymbols(Symbols = "^GSPC", src = "yahoo", from = "1986-01-01", 
                   to = "2020-01-01", auto.assign = F)
SP500 = SP500$GSPC.Adjusted
rSP500 = zoo(diff(log(as.vector(SP500))), time(SP500)[-1])

#prepare axis values
timeLAB = 1986:2020
timeAT = seq(as.Date("1986-01-01"), as.Date("2020-01-01"), by = "year")

#custom plot.zoo to plot every year a tick on the x-axis
plot.zoo2 <- plot.zoo
bl <- as.list(body(plot.zoo2))
bl <- c(
  bl[1:19], 
  quote(axis(1, at = timeAT, labels = timeLAB)), 
  bl[20]
)
body(plot.zoo2) <- as.call(bl)

#create plot
plot.zoo2(cbind("DJIA" = rDJ, "S&P500" = rSP500), col = as.list(c("darkblue", "blue")),
          main = "", ylim = c(-0.25, 0.115), xlab = "Time", xaxt = "n")
