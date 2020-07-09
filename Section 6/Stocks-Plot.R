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

plot.ts(cbind(rDJ, rSP500), xaxt = "n", col = c("blue3", "red3"), plot.type = "single",
        ylab = "log-returns")
axis(1, at = timeAT, labels = timeLAB)
legend("bottom", col = c("blue3", "red3"), bty = "n", legend = c("DJ", "S&P500"),
       border = rep(NA, 2), lty = rep(1, 2), density = rep(0, 2), lwd = 2)
