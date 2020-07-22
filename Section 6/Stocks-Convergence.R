#Note: The calculations in this script take approximately 15 minutes on an Intel(R) Xeon(R)
#      Gold 6136 CPU with 3.00 GHz on a 64-Bit Windows Server 2016 with 24 processors.

library("quantmod")
library("rugarch")
library("copula")

#load data
SP500 = getSymbols(Symbols = "^GSPC", src = "yahoo", from = "1986-01-01", 
                   to = "2020-01-01", auto.assign = F)
rSP500 = diff(log(as.vector(SP500$GSPC.Adjusted)))
DJ = getSymbols(Symbols = "^DJI", src = "yahoo", from = "1986-01-01", 
                to = "2020-01-01", auto.assign = F)
rDJ = diff(log(as.vector(DJ$DJI.Adjusted)))

#define functions
get_BivaCoVaR = function(theta, df, fitY, alpha = 0.05, beta = 0.05){
        MinF = function(v){
                return(pCopula(c(v, alpha), copula = tCopula(param = theta, df = df)) - alpha*beta)
        }
        tmp = uniroot(f = MinF, lower = 0, upper = 1, tol = .Machine$double.eps*0.0000001, maxiter = 10000)$root
        CoVaR = as.vector(quantile(fitY, tmp))
        return(CoVaR)
}

evalBivariate = function(retX, retY, VaRX, CoVaR){
        exc = which(retX <= VaRX)
        
        red_CoVaR = rep(NA, length(CoVaR))
        red_CoVaR[exc] = CoVaR[exc]
        CoVaR_exc = which(retY <= red_CoVaR)
        CoVaR_rate = length(CoVaR_exc)/length(exc)
        
        return("CoVaR_rate" = CoVaR_rate)
}

Pipe = function(retY, retX, fitX, fixed.pars, alpha = 0.05, beta = 0.05){
        specY = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                            mean.model = list(armaOrder = c(0,0), include.mean = F), 
                            distribution.model = "std", fixed.pars = fixed.pars)
        filterY = ugarchfilter(spec = specY, data = retY)
        
        v = as.vector(pit(filterY))
        u = as.vector(pit(fitX))
        
        #calculate VaR
        VaRX = as.vector(quantile(fitX, alpha))
        
        #copula estimate
        cop = fitCopula(copula = tCopula(), data = cbind(v, u), method = "ml", estimate.variance = F)
        
        #calculate CoVaR
        CoVaR = get_BivaCoVaR(theta = cop@estimate[1], df = round(cop@estimate[2]), 
                              fitY = filterY, alpha = alpha, beta = beta)
        #evaluate
        CoVaR_rate = evalBivariate(retX = rSP500, retY = rDJ, VaRX = VaRX, CoVaR = CoVaR)
        return(CoVaR_rate)
}

set.seed(713)

# Step 1: Estimate GARCH(1,1)-t for DJ and S&P500
spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = F), 
                    distribution.model = "std")
fitDJ = ugarchfit(spec = spec, data = rDJ, solver = "hybrid")
fitSP500 = ugarchfit(spec = spec, data = rSP500, solver = "hybrid")

# Step 2: Calculate CoVaR rate with t-copula
tcop = fitCopula(copula = tCopula(), data = cbind(as.vector(pit(fitDJ)), as.vector(pit(fitSP500))), 
                 method = "ml", estimate.variance = F)
CoVaR = get_BivaCoVaR(theta = tcop@estimate[1], df = round(tcop@estimate[2]),
                      fitY = fitDJ, alpha = 0.05, beta = 0.05)
CoVaR_rate = evalBivariate(retX = rSP500, retY = rDJ, VaRX = as.vector(quantile(fitSP500, 0.05)), CoVaR = CoVaR)

# Step 3: Fix parameter estimates of the GARCH-dynamics of DJ and calculate violation rate with normal innovations
coefFIX = coef(fitDJ)[c("mu", "omega", "alpha1", "beta1")]
specDJnorm = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                        mean.model = list(armaOrder = c(0,0), include.mean = F), 
                        distribution.model = "norm", fixed.pars = as.list(coefFIX))
filterDJ = ugarchfilter(spec = specDJnorm, data = rDJ)

tcop_new = fitCopula(copula = tCopula(), data = cbind(as.vector(pit(filterDJ)), as.vector(pit(fitSP500))), 
                     method = "ml", estimate.variance = F)
CoVaR_new = get_BivaCoVaR(theta = tcop_new@estimate[1], df = round(tcop_new@estimate[2]),
                          fitY = filterDJ, alpha = 0.05, beta = 0.05)
CoVaR_rate_new = evalBivariate(retX = rSP500, retY = rDJ, VaRX = as.vector(quantile(fitSP500, 0.05)), CoVaR = CoVaR_new)

# Step 4: Fix parameter estimates of the GARCH-dynamics of DJ and calculate violation rate for different t-innovations
paramSEQ = seq(coef(fitDJ)["shape"], to = 350, length.out = 70)

t_rates = c()
for(i in seq_along(paramSEQ)){
        t_rates[i] = Pipe(retY = rDJ, retX = rSP500, fitX = fitSP500, 
                          fixed.pars = as.list(c(coefFIX, "shape" = paramSEQ[i])))
        cat(i)
}

#plot
plot(paramSEQ, t_rates, pch = 19, col = "blue3", xlab = "v", ylab = "CoVaR violation rate", las = 1, ylim = c(0, 0.17))
abline(h = CoVaR_rate, col = "red3")
abline(h = CoVaR_rate_new, col = "red3")
