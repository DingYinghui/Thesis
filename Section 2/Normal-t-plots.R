#Note: The calculations in this script take only a couple of seconds on an Intel(R) Xeon(R) 
#      Gold 6136 CPU with 3.00 GHz on a 64-Bit Windows Server 2016 with 24 processors.

library("copula")
par(mfrow = c(1,2), mai = c(1, 0.5, 0.5, 0.1))

#normal copula: tau = 0.3
set.seed(50)
n1 = iTau(normalCopula(), tau = 0.3)
plot(rCopula(n = 500, copula = normalCopula(n1)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x1 <- mvdc(copula = normalCopula(param = n1), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x1, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")

#normal copula: tau = 0.7
set.seed(51)
n2 = iTau(normalCopula(), tau = 0.7)
plot(rCopula(n = 500, copula = normalCopula(n2)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x2 <- mvdc(copula = normalCopula(param = n2), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x2, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")

#t-copula: tau = 0.3
set.seed(52)
t1 = iTau(tCopula(), tau = 0.3)
plot(rCopula(n = 500, copula = tCopula(t1, df = 4)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x3 <- mvdc(copula = tCopula(param = t1, df = 4), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x3, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")

#t-copula: tau = 0.7
set.seed(53)
t2 = iTau(tCopula(), tau = 0.7)
plot(rCopula(n = 500, copula = tCopula(t2, df = 4)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x4 <- mvdc(copula = tCopula(param = t2, df = 4), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x4, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")
