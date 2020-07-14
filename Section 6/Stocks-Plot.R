#Note: The calculations in this script take only a couple of seconds on an Intel(R) Xeon(R)
#      Gold 6136 CPU with 3.00 GHz on a 64-Bit Windows Server 2016 with 24 processors.

library("quantmod")
library("lubridate")

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

#create plot
time = as.character(floor_date(time(SP500[-1]), unit = "year"))
timeLAB = 1986:2020

timeAT = c()
for(i in seq_along(timeLAB[-length(timeLAB)])){
        tmp = which(startsWith(time, as.character(timeLAB[i])))[1]
        timeAT = c(timeAT, tmp)
}
timeAT = c(timeAT, length(SP500))

plot.ts(cbind("DJIA" = rDJ, "S&P500" = rSP500), xaxt = "n", col = "blue3",
        oma.multi = c(5.1,0,6,0), main = "")
axis(1, at = timeAT, labels = timeLAB)
