#Note: The calculations in this script take approximately 10 minutes on an Intel(R) Xeon(R)
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
get_BivaCoVaR = function(copula, theta, df = NULL, fitY, alpha = 0.05, beta = 0.05){
        if(copula == "normal"){
                MinF = function(v){
                        return(pCopula(c(v, alpha), copula = normalCopula(param = theta)) - alpha*beta)
                }
        }
        if(copula == "t"){
                MinF = function(v){
                        return(pCopula(c(v, alpha), copula = tCopula(param = theta, df = df)) - alpha*beta)
                }
        }
        if(copula == "clayton"){
                MinF = function(v){
                        return(pCopula(c(v, alpha), copula = claytonCopula(param = theta)) - alpha*beta)
                }
        }
        if(copula == "gumbel"){
                MinF = function(v){
                        return(pCopula(c(v, alpha), copula = gumbelCopula(param = theta)) - alpha*beta)
                }
        }
        tmp = uniroot(f = MinF, lower = 0, upper = 1, tol = .Machine$double.eps*0.0000001, maxiter = 10000)$root
        CoVaR = as.vector(quantile(fitY, tmp))
        
        return(CoVaR)
}

evalBivariate = function(retX, retY, VaRX, VaRY, CoVaR){
        exc = which(retX <= VaRX)
        
        red_CoVaR = rep(NA, length(CoVaR))
        red_CoVaR[exc] = CoVaR[exc]
        CoVaR_exc = which(retY <= red_CoVaR)
        CoVaR_rate = length(CoVaR_exc)/length(exc)
        
        return(c("VaRY_rate" = length(which(retY <= VaRY))/length(retY),
                 "VaRX_rate" = length(exc)/length(retX),
                 "CoVaR_rate" = CoVaR_rate))
}

RatePipe = function(retY, retX, copula, AROrder, MAOrder, garchModel, 
                    margin.dist, alpha = 0.05, beta = 0.05){
        if(!is.element(copula, c("normal", "t", "clayton", "gumbel"))){
                stop("Please insert one of the considered copulae.")}
        retY = as.vector(retY)
        retX = as.vector(retX)
        
        #Margin Modeling
        spec = ugarchspec(mean.model = list(armaOrder = c(AROrder, MAOrder), include.mean = F), 
                          variance.model = list(model = garchModel, garchOrder = c(1,1)),
                          distribution.model = margin.dist)
        fitY = ugarchfit(spec = spec, data = retY, solver = "hybrid")
        fitX = ugarchfit(spec = spec, data = retX, solver = "hybrid")
        v = as.vector(pit(fitY))
        u = as.vector(pit(fitX))
        
        #calculate VaR
        VaRY = as.vector(quantile(fitY, alpha))
        VaRX = as.vector(quantile(fitX, alpha))
        
        #Copula and CoVaR estimation
        if(copula == "normal"){
                copEST = fitCopula(copula = normalCopula(), data = cbind(v, u), 
                                   method = "ml", estimate.variance = F)
                CoVaR = get_BivaCoVaR(theta = copEST@estimate, fitY = fitY, copula = "normal",
                                      alpha = alpha, beta = beta)
        }
        if(copula == "t"){
                copEST = fitCopula(copula = tCopula(), data = cbind(v, u), 
                                   method = "ml", estimate.variance = F)
                CoVaR = get_BivaCoVaR(theta = copEST@estimate[1], df = round(copEST@estimate[2]),
                                      fitY = fitY, copula = "t", alpha = alpha, beta = beta)
        }
        if(copula == "clayton"){
                copEST = fitCopula(copula = claytonCopula(), data = cbind(v, u), 
                                   method = "ml", estimate.variance = F)
                CoVaR = get_BivaCoVaR(theta = copEST@estimate, fitY = fitY, copula = "clayton",
                                      alpha = alpha, beta = beta)
        }
        if(copula == "gumbel"){
                copEST = fitCopula(copula = gumbelCopula(), data = cbind(v, u), 
                                   method = "ml", estimate.variance = F)
                CoVaR = get_BivaCoVaR(theta = copEST@estimate, fitY = fitY, copula = "gumbel",
                                      alpha = alpha, beta = beta)
        }
        
        #evaluate
        rates = evalBivariate(retY = retY, retX = retX, VaRX = VaRX, VaRY = VaRY, CoVaR = CoVaR)
        return(round(rates,4))
}

set.seed(435)

#table with considered models
tbl = as.data.frame(cbind("AROrder" = c(rep(c(rep(1,4), rep(0,8)), 3)),
                          "MAOrder" = c(rep(c(rep(1,4), rep(0,8)),3)),
                          "garchModel" = c(rep(c(rep("sGARCH",8), rep("gjrGARCH",4)),3)),
                          "margin.dist" = c(rep("norm", 12), rep("sged", 12), rep("sstd", 12)),
                          "copula" = rep(c("normal", "t", "clayton", "gumbel"),9)))
for(i in 1:2){
        tbl[,i] = as.numeric(tbl[,i])
}

res = rep(NA, 3)
for(i in seq_len(nrow(tbl))){
        tmp = RatePipe(retY = rDJ, retX = rSP500, margin.dist = tbl$margin.dist[i], 
                       AROrder = tbl$AROrder[i], MAOrder = tbl$MAOrder[i], garchModel = tbl$garchModel[i],
                       copula = tbl$copula[i])
        res = rbind(res, tmp)
        cat(i)
}
res = res[-1,]
tbl = cbind(tbl, res)
