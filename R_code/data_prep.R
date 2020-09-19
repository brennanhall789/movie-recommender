library(xts)
library(stochvol)
library(rugarch)
library(sarima)
library(quantmod)


getSymbols("NFLX", from = "2017-1-3", to = "2018-10-28")
head(NFLX)
tail(NFLX)
length(NFLX$NFLX.Open)
plot(NFLX$NFLX.Close, main = "NFLX Close Price")

NFLX_past <- NFLX$NFLX.Close
NFLX_pres <- NFLX$NFLX.Close
n_past <- length(NFLX_past)
n_pres <- length(NFLX_pres)
logrets_past <- logret(NFLX_past, demean = TRUE)
logrets_pres <- logret(NFLX_pres, demean = TRUE)

#######
# PAST
#######
 
plot(logrets_past, col="blue", lty=1, lwd=1, main="Log-Returns")


par(mfrow=c(1,2))
acf1.past <- acf(logrets_past, lag.max = 200, main=expression(paste("ACF of ", X[t])))
pacf1.past <- pacf(logrets_past, lag.max = 200, main=expression(paste("PACF of ", X[t])))
acf2.past <- acf(logrets_past^2, lag.max = 200, main=expression(paste("ACF of ", X[t]^2)))
pacf2.past <- pacf(logrets_past^2, lag.max = 200, main=expression(paste("PACF of ", X[t]^2)))

Box.test(coredata(logrets_past^2),type="Ljung-Box")

#######
# PRES
#######

y.pres <- logret(NFLX_pres, demean = T)
plot(y.pres, col="blue", lty=1, lwd=1, main="Log-Returns")

acf1.pres <- acf(y.pres, lag.max = 200)
pacf1.pres <- pacf(y.pres, lag.max = 200)
acf2.pres <- acf(y.pres^2, lag.max = 200)
pacf2.pres <- pacf(y.pres^2, lag.max = 200)



texreg(summ)
xtable(summ)
