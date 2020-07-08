#Note: The calculations in this script take only a couple of seconds on an Intel(R) Xeon(R) 
#      Gold 6136 CPU with 3.00 GHz on a 64-Bit Windows Server 2016 with 24 processors.

library("copula")
par(mfrow = c(1,2), mai = c(1, 0.5, 0.5, 0.1))

#Gumbel copula: tau = 0.3
set.seed(50)
g1 = iTau(gumbelCopula(), tau = 0.3)
plot(rCopula(n = 500, copula = gumbelCopula(g1)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x1 <- mvdc(copula = gumbelCopula(param = g1), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x1, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")

#Gumbel copula: tau = 0.7
set.seed(51)
g2 = iTau(gumbelCopula(), tau = 0.7)
plot(rCopula(n = 500, copula = gumbelCopula(g2)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x2 <- mvdc(copula = gumbelCopula(param = g2), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x2, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")

#Clayton copula: tau = 0.3
set.seed(52)
c1 = iTau(claytonCopula(), tau = 0.3)
plot(rCopula(n = 500, copula = claytonCopula(c1)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x3 <- mvdc(copula = claytonCopula(param = c1), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x3, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")

#Clayton copula: tau = 0.7
set.seed(53)
c2 = iTau(claytonCopula(), tau = 0.7)
plot(rCopula(n = 500, copula = claytonCopula(c2)), xlab = "", ylab = "", 
     las = 1, pch = 19, col = "blue3")
x4 <- mvdc(copula = claytonCopula(param = c2), margins = c("norm", "norm"), 
           paramMargins = list(list(mean = 0, sd =1), list(mean = 0, sd = 1)))
contour(x4, dMvdc, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), las = 1, col = "blue3", 
        lwd = 2, xlab = "", ylab = "")
