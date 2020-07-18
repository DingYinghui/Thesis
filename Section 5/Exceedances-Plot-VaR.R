#Note: The calculations in this script take only a couple of seconds on an Intel(R) Xeon(R)
#      Gold 6136 CPU with 3.00 GHz on a 64-Bit Windows Server 2016 with 24 processors.

library("rugarch")
library("zoo")

#load BTC, LTC, XMR, XRP from 'https://github.com/MarWaltz/Thesis/tree/master/Section 5/Data'
rBTC = diff(log(BTC))
rLTC = diff(log(LTC))
rXMR = diff(log(XMR))
rXRP = diff(log(XRP))
rSys = rLTC + rXMR + rXRP

set.seed(312)

################################### Return Margin Modeling ########################################
#BTC
specrBTC = ugarchspec(mean.model = list(armaOrder = c(1,1), include.mean = 1),
                      variance.model = list(model = "gjrGARCH", garchOrder = c(3,0)),
                      distribution.model = "sstd")
fitrBTC = ugarchfit(spec = specrBTC, data = rBTC, solver = "hybrid")
v = as.vector(pit(fitrBTC))

#LTC
specrLTC = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 0),
                      variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                      distribution.model = "sstd")
fitrLTC = ugarchfit(spec = specrLTC, data = rLTC, solver = "hybrid")
u1 = as.vector(pit(fitrLTC))

#XMR
specrXMR = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 0),
                      variance.model = list(model = "sGARCH", garchOrder = c(6,1)),
                      distribution.model = "sstd")
fitrXMR = ugarchfit(spec = specrXMR, data = rXMR, solver = "hybrid")
u2 = as.vector(pit(fitrXMR))

#XRP
specrXRP = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 1),
                      variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                      distribution.model = "sstd")
fitrXRP = ugarchfit(spec = specrXRP, data = rXRP, solver = "hybrid")
u3 = as.vector(pit(fitrXRP))

#System
specrSys = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 0),
                      variance.model = list(model = "sGARCH", garchOrder = c(6,0)),
                      distribution.model = "sstd")
fitrSys = ugarchfit(spec = specrSys, data = rSys, solver = "hybrid")
u4 = as.vector(pit(fitrSys))

####################################### UNIVARIATE VAR ############################################
VaR_rBTC = as.vector(fitted(fitrBTC)) + as.vector(sigma(fitrBTC))*qdist("sstd", 0.05, shape = coef(fitrBTC)["shape"], skew = coef(fitrBTC)["skew"])
VaR_rLTC = as.vector(fitted(fitrLTC)) + as.vector(sigma(fitrLTC))*qdist("sstd", 0.05, shape = coef(fitrLTC)["shape"], skew = coef(fitrLTC)["skew"])
VaR_rXMR = as.vector(fitted(fitrXMR)) + as.vector(sigma(fitrXMR))*qdist("sstd", 0.05, shape = coef(fitrXMR)["shape"], skew = coef(fitrXMR)["skew"])
VaR_rXRP = as.vector(fitted(fitrXRP)) + as.vector(sigma(fitrXRP))*qdist("sstd", 0.05, shape = coef(fitrXRP)["shape"], skew = coef(fitrXRP)["skew"])
VaR_rSys = as.vector(fitted(fitrSys)) + as.vector(sigma(fitrSys))*qdist("sstd", 0.05, shape = coef(fitrSys)["shape"], skew = coef(fitrSys)["skew"])

#BTC
VaR_BTC_exc = which(rBTC <= VaR_rBTC)
VaR_BTC_exc_rate = length(VaR_BTC_exc)/length(rBTC)
VaR_BTC_diff = rep(0, length(rBTC))
VaR_BTC_diff[VaR_BTC_exc] = abs(rBTC[VaR_BTC_exc] - VaR_rBTC[VaR_BTC_exc])

#LTC
VaR_LTC_exc = which(rLTC <= VaR_rLTC)
VaR_LTC_exc_rate = length(VaR_LTC_exc)/length(rLTC)
VaR_LTC_diff = rep(0, length(rLTC))
VaR_LTC_diff[VaR_LTC_exc] = abs(rLTC[VaR_LTC_exc] - VaR_rLTC[VaR_LTC_exc])

#XMR
VaR_XMR_exc = which(rXMR <= VaR_rXMR)
VaR_XMR_exc_rate = length(VaR_XMR_exc)/length(rXMR)
VaR_XMR_diff = rep(0, length(rXMR))
VaR_XMR_diff[VaR_XMR_exc] = abs(rXMR[VaR_XMR_exc] - VaR_rXMR[VaR_XMR_exc])

#XRP
VaR_XRP_exc = which(rXRP <= VaR_rXRP)
VaR_XRP_exc_rate = length(VaR_XRP_exc)/length(rXRP)
VaR_XRP_diff = rep(0, length(rXRP))
VaR_XRP_diff[VaR_XRP_exc] = abs(rXRP[VaR_XRP_exc] - VaR_rXRP[VaR_XRP_exc])

#System
VaR_Sys_exc = which(rSys <= VaR_rSys)
VaR_Sys_exc_rate = length(VaR_Sys_exc)/length(rSys)
VaR_Sys_diff = rep(0, length(rSys))
VaR_Sys_diff[VaR_Sys_exc] = abs(rSys[VaR_Sys_exc] - VaR_rSys[VaR_Sys_exc])

############################################ PLOT ###############################################

#need to modify plot.zoo function as follows:
plot.zoo = function (x, y = NULL, screens, plot.type, panel = lines, xlab = "Index", 
                     ylab = NULL, main = NULL, xlim = NULL, ylim = NULL, xy.labels = FALSE, 
                     xy.lines = NULL, yax.flip = FALSE, oma = c(6, 0, 5, 0), mar = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1), 
                     col = 1, lty = 1, lwd = 1, pch = 1, type = "l", log = "", nc, widths = 1, 
                     heights = 1, ...) 
{
  if (!is.null(y)) {
    if (NCOL(x) > 1 || NCOL(y) > 1) 
      stop("scatter plots only for univariate zoo series")
    xyzoo <- merge.zoo(x, y, all = FALSE)
    xy <- coredata(xyzoo)
    xy <- xy.coords(xy[, 1], xy[, 2])
    xlab <- if (missing(xlab)) 
      deparse(substitute(x))
    else xlab
    ylab <- if (missing(ylab)) 
      deparse(substitute(y))
    else ylab
    xlim <- if (is.null(xlim)) 
      range(xy$x[is.finite(xy$x)])
    else xlim
    ylim <- if (is.null(ylim)) 
      range(xy$y[is.finite(xy$y)])
    else ylim
    if (is.null(main)) 
      main <- ""
    do.lab <- if (is.logical(xy.labels)) 
      xy.labels
    else TRUE
    if (is.null(xy.lines)) 
      xy.lines <- do.lab
    ptype <- if (do.lab) 
      "n"
    else if (missing(type)) 
      "p"
    else type
    plot.default(xy, type = ptype, col = col, pch = pch, 
                 main = main, xlab = xlab, ylab = ylab, xlim = xlim, 
                 ylim = ylim, log = log, ...)
    if (do.lab) 
      text(xy, col = col, labels = if (!is.logical(xy.labels)) 
        xy.labels
        else index2char(index(xyzoo)), ...)
    if (xy.lines) 
      lines(xy, col = col, lty = lty, lwd = lwd, type = if (do.lab) 
        "c"
        else "l", ...)
    return(invisible(xyzoo))
  }
  recycle <- function(a, len, nser) rep(lapply(as.list(a), 
                                               rep, length.out = len), length.out = nser)
  range2 <- function(x, ...) if (length(x) == 2) 
    x
  else range(x, ...)
  if (missing(plot.type)) {
    plot.type <- if (missing(screens)) 
      "multiple"
    else if (length(unique(screens) == 1)) 
      "single"
    else "multiple"
  }
  plot.type <- match.arg(plot.type, c("multiple", "single"))
  nser <- NCOL(x)
  if (missing(screens)) {
    screens <- if (plot.type == "single") 
      1
    else seq_len(nser)
  }
  dots <- list(...)
  x.index <- index(x)
  if (is.ts(x.index)) 
    x.index <- as.vector(x.index)
  cn <- if (is.null(colnames(x))) 
    paste("V", seq_len(nser), sep = "")
  else colnames(x)
  screens <- make.par.list(cn, screens, NROW(x), nser, 1)
  screens <- as.factor(unlist(screens))[drop = TRUE]
  ngraph <- length(levels(screens))
  if (nser > 1 && (plot.type == "multiple" || ngraph > 
                   1)) {
    if (ngraph == 1) {
      screens <- as.factor(seq(nser))
      ngraph <- nser
    }
    if (is.null(main)) 
      main <- deparse(substitute(x))
    main.outer <- TRUE
    if (is.null(ylab)) 
      ylab <- colnames(x)[!duplicated(screens)]
    if (is.null(ylab)) 
      ylab <- paste("Series", which(!duplicated(screens)))
    if (is.call(ylab)) 
      ylab <- as.expression(ylab)
    ylab <- rep(ylab, length.out = ngraph)
    if (!is.list(ylab)) 
      ylab <- as.list(ylab)
    lty <- rep(lty, length.out = nser)
    lwd <- rep(lwd, length.out = nser)
    col <- make.par.list(cn, col, NROW(x), nser, 1)
    pch <- make.par.list(cn, pch, NROW(x), nser, par("pch"))
    type <- make.par.list(cn, type, NROW(x), nser, "l")
    if (!is.null(ylim)) {
      if (is.list(ylim)) 
        ylim <- lapply(ylim, range2, na.rm = TRUE)
      else ylim <- list(range2(ylim, na.rm = TRUE))
      ylim <- lapply(make.par.list(cn, ylim, 2, nser, NULL), 
                     function(x) if (is.null(x) || length(na.omit(x)) == 
                                     0) 
                       NULL
                     else range2(x, na.rm = TRUE))
    }
    panel <- match.fun(panel)
    if (missing(nc)) 
      nc <- if (ngraph > 4) 
        2
    else 1
    oldpar <- par(no.readonly = TRUE)
    on.exit({
      par(oldpar)
    })
    nr <- ceiling(ngraph/nc)
    layout(matrix(seq(nr * nc), nr), widths = widths, heights = heights)
    par(mar = mar, oma = oma)
    allsame <- function(L) {
      f <- function(x, y) if (identical(x, y)) 
        x
      !is.null(Reduce(f, L))
    }
    f <- function(idx) if (allsame(ylim)) 
      ylim[idx][[1]]
    else if (!is.null(ylim) && length(idx) > 0 && length(unlist(ylim[idx])) > 
             0) 
      range(ylim[idx], finite = TRUE)
    else range(x[, idx], na.rm = TRUE)
    ranges <- tapply(1:ncol(x), screens, f)
    for (j in seq_along(levels(screens))) {
      panel.number <- j
      y.side <- if (j%%2 || !yax.flip) 
        2
      else 4
      range. <- rep(ranges[[j]], length.out = length(time(x)))
      if (j%%nr == 0 || j == length(levels(screens))) {
        args <- list(x.index, range., xlab = "", 
                     ylab = "", yaxt = "n", xlim = xlim, 
                     ylim = ylim[[j]], log = log, ...)
        args$type <- "n"
        do.call("plot", args)
        mtext(xlab, side = 1, line = 3)
      }
      else {
        args <- list(x.index, range., xaxt = "n", 
                     yaxt = "n", xlab = "", ylab = "", 
                     xlim = xlim, ylim = ylim[[j]], log = log, ...)
        args$type <- "n"
        do.call("plot", args)
        if ("bty" %in% names(args) && args$bty == 
            "n") {
        }
        else box()
      }
      do.call("axis", c(list(side = y.side, xpd = NA), 
                        dots))
      
      corners = par("usr")
      par(xpd = TRUE)
      text(x = corners[1]-50, y = mean(corners[3:4])-0.1, ylab[[j]], cex = 1.3)
      
      for (i in which(screens == levels(screens)[j])) {
        series.number <- i
        series.within.screen <- ave(seq_along(screens), 
                                    screens, FUN = seq_along)[series.number]
        panel(x.index, x[, i], col = col[[i]], pch = pch[[i]], 
              lty = lty[i], lwd = lwd[i], type = type[[i]], 
              ...)
      }
    }
  }
  else {
    if (is.null(ylab)) 
      ylab <- deparse(substitute(x))
    if (is.call(ylab)) 
      ylab <- as.expression(ylab)
    if (is.null(main)) 
      main <- ""
    main.outer <- FALSE
    if (is.null(ylim)) 
      ylim <- range(x, na.rm = TRUE)
    else ylim <- range2(c(ylim, recursive = TRUE), na.rm = TRUE)
    lty <- rep(lty, length.out = nser)
    lwd <- rep(lwd, length.out = nser)
    col <- make.par.list(cn, col, NROW(x), nser, 1)
    pch <- make.par.list(cn, pch, NROW(x), nser, par("pch"))
    type <- make.par.list(cn, type, NROW(x), nser, "l")
    dummy <- rep(range(x, na.rm = TRUE), length.out = length(index(x)))
    args <- list(x.index, dummy, xlab = xlab, ylab = ylab[1], 
                 ylim = ylim, xlim = xlim, log = log, ...)
    args$type <- "n"
    do.call("plot", args)
    if ("bty" %in% names(args) && args$bty == "n") {
    }
    else box()
    y <- as.matrix(x)
    for (i in 1:nser) {
      panel(x.index, y[, i], col = col[[i]], pch = pch[[i]], 
            lty = lty[i], lwd = lwd[i], type = type[[i]], 
            ...)
    }
  }
  dots <- list(...)
  title.args <- c(list(main = main, outer = main.outer), dots[grep("[.]main$", 
                                                                   names(dots))])
  do.call("title", title.args)
  return(invisible(x))
}

VecToTS = function(vec, start = "2014-09-02", end = "2020-04-19"){
  inds <- seq(as.Date(start), as.Date(end), by = "day")
  return(zoo(as.vector(vec), inds))
}
res = cbind(VecToTS(VaR_BTC_diff), VecToTS(VaR_LTC_diff), 
            VecToTS(VaR_XMR_diff), VecToTS(VaR_XRP_diff), 
            VecToTS(VaR_Sys_diff))
  
#standardize VaR exceedances
res = res/max(res)

#plot
plot(res, ylab = c("BTC", "LTC", "XMR", "XRP", "System"), nc = 1, yaxt = "n", las = 1, bty = "n",
     ylim = c(0,1), cex.axis = 1.25, cex.lab = 1.35, yaxs = "i", xlab = "Time", main = "", col = "blue3", las = 1)
