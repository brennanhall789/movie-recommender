library(MASS)
library(forecast)
library(Metrics)
library(multitaper)

log.t.past <- 1:(n_past-1)
log.t.pres <- 1:(n_pres-1)


###############
### PAST
###############

plot(NFLX_past)


acf(diff(NFLX_past)[-1], lag.max = 200)
pacf(diff(NFLX_past)[-1], lag.max = 200)

b=boxcox(NFLX_past ~ as.numeric(1:n_past)) ## indicates inverse transform
title("Box-Cox transformation")

nflx_past_sqr <- NFLX_past^(-1)
plot(nflx_past_sqr)
acf(nflx_past_sqr, lag.max=200)
pacf(nflx_past_sqr, lag.max = 200)

nflx_past_sqr1 <- diff(nflx_past_sqr)
plot(nflx_past_sqr1, main="Transformed Lag-1 NFLX Price")
par(mfrow=c(1,2))
acf(nflx_past_sqr1[-1], lag.max = 200, main="ACF of Series")
pacf(nflx_past_sqr1[-1], lag.max = 200, main="PACF of Series") ### indicates ARIMA(3,1,0)? for inverse transform
                                          ## but nothing conclusive -> use stepwise selection

acf(NFLX_past^2, lag.max = 200, main="ACF of Series")
pacf(NFLX_past^2, lag.max = 200, main="PACF of Series") ### indicates ARIMA(3,1,0)? for inverse transform


nflx_past_log <- log(NFLX_past)
plot(nflx_past_log)

plot(logrets_past)

arma_acf_past <- acf(logrets_past, lag.max = 200)
arma_pacf_past <- pacf(logrets_past, lag.max = 200)  ## also indicates ARIMA(3,1,0) for logrets



##########
### ARMA
##########

arma_fit_sqrt_past <- auto.arima(NFLX_past, d=1, max.order = 7, ic = "aicc", trace = T,
                                 lambda=-1, stepwise = F, num.cores = NULL, allowdrift=F, allowmean=F)
summ <- summary(arma_fit_sqrt_past)

arma_fit_log_past <- auto.arima(NFLX_past, d=1, max.order = 7, ic = "aic",
                                lambda = 0, stepwise = T, allowdrift=F, allowmean=F)
summary(arma_fit_log_past)

### Conclude: use inverse fit b/c much higher log.lik/smaller AIC and similar error measurements
## and same size model

plot(c(as.numeric(NFLX_past), as.numeric(NFLX_pres)), type='l',
     ylab=expression(X[t]))
legend('topleft', legend=c(expression(X[t]), "Fitted/Forecast", "Upper 95% CI", "Lower 95% CI"),
       col = c("black", "red","green","blue"),lty=1)
lines(c(arma_fit_sqrt_past$fitted, forecast_arma_sqrt_past$mean),col='red')
lines(x=c(459:481),forecast_arma_sqrt_past$lower[-1,2],col='blue')
lines(x=c(459:481),forecast_arma_sqrt_past$upper[-1,2],col='green')
polygon(c(c(459:481), rev(c(459:481))), 
        c(forecast_arma_sqrt_past$upper[-1,2], rev(forecast_arma_sqrt_past$lower[-1,2])),
        col = rgb(0,0,0,.2), border = NA)

rmse(actual = nflx_past_sqr1[-1], predicted = arma_fit_sqrt_past$fitted[-1])
mae(actual = nflx_past_sqr1[-1], predicted = arma_fit_sqrt_past$fitted[-1])
mape(actual = nflx_past_sqr1[-1], predicted = arma_fit_sqrt_past$fitted[-1])
mase(actual = nflx_past_sqr1[-1], predicted = arma_fit_sqrt_past$fitted[-1])

##########
## stat/invert
##########
plot(arma_fit_sqrt_past)

plot(arma_fit_log_past)

####
## Diagnostics 
####
Box.test(residuals(arma_fit_sqrt_past), type = "Ljung") ## passes independence
plot(sqrt(abs(residuals(arma_fit_sqrt_past))) ~ fitted(arma_fit_sqrt_past)) # indicates NcV
shapiro.test(residuals(arma_fit_sqrt_past)) ## fails normality
qqnorm(arma_fit_sqrt_past$residuals)
qqline(arma_fit_sqrt_past$residuals)


Box.test(nflx_past_sqr1,lag=3, type = "Ljung") ## passes independence
plot(sqrt(abs(residuals(arma_fit_log_past))) ~ fitted(arma_fit_log_past),
     main="Standardized Residuals vs Fitted plot", ylab = expression(sqrt(abs(Residuals))), xlab="Fitted values") ## indicates NcV
shapiro.test(residuals(arma_fit_log_past)) ## fails normality
qqnorm(arma_fit_log_past$residuals)
qqline(arma_fit_log_past$residuals)


ts.plot(arma_fit_sqrt_past$fitted)
ts.plot(arma_fit_sqrt_past$residuals)


ts.plot(arma_fit_log_past$fitted)
ts.plot(arma_fit_log_past$residuals)


########
## Forecast accuracy
########

# plot(x=as.Date(index(NFLX$NFLX.Close[340:419])),as.vector(NFLX$NFLX.Close[340:419]),type='l', ylim = range(c(NFLX$NFLX.Close[340:419]), forecast_arma_log_past$mean))
# lines(x=as.Date(p[,1]), forecast_arma_sqrt_past$mean)


forecast_arma_sqrt_past <- forecast(arma_fit_sqrt_past, h = 23)
er <- forecast_arma_sqrt_past$mean - as.numeric(NFLX_pres)

plot(forecast_arma_sqrt_past,xlab = "Index", ylab = expression(S[t]))
lines(NFLX_pres, col="r")
plot(er, ylab="Forecast error", xlab="Index", main="Forecast error of ARIMA(3,1,0) model")

DACTest(forecast = forecast_arma_sqrt_past$mean, actual = as.numeric(NFLX_pres),test="PT")

rmse_arma_sqr <- rmse(predicted = as.numeric(forecast_arma_sqrt_past$mean), actual = NFLX_pres)
mae_arma_sqr <- mae(predicted = as.numeric(forecast_arma_sqrt_past$mean), actual = NFLX_pres)
mape_arma_sqr <- smape(predicted = as.numeric(forecast_arma_sqrt_past$mean), actual = NFLX_pres)
sse_arma_sqr <- sse(predicted = as.numeric(forecast_arma_sqrt_past$mean), actual = NFLX_pres)
(acc_arma_sqr <- data.frame(RMSE = rmse_arma_sqr, MAE = mae_arma_sqr, SMAPE = mape_arma_sqr, SSE = sse_arma_sqr))
xtable(acc_arma_sqr)

forecast_arma_log_past <- forecast(arma_fit_log_past, h = 23)
er_log <- forecast_arma_log_past$mean - as.numeric(NFLX_pres)

plot(forecast_arma_log_past)
plot(er_log)

rmse_arma_log <- rmse(predicted = as.numeric(forecast_arma_log_past$mean), actual = NFLX_pres)
mae_arma_log <- mae(predicted = as.numeric(forecast_arma_log_past$mean), actual = NFLX_pres)
mape_arma_log <- mape(predicted = as.numeric(forecast_arma_log_past$mean), actual = NFLX_pres)
sse_arma_log <- sse(predicted = as.numeric(forecast_arma_log_past$mean), actual = NFLX_pres)
(acc_arma_log <- data.frame(RMSE = rmse_arma_log, MAE = mae_arma_log, MAPE = mape_arma_log, SSE = sse_arma_log))


##############
## Spectral
##############
arma_fit_sqrt_past$coef
ar <- arma_fit_sqrt_past$coef[1:3]
ma <- arma_fit_sqrt_past$coef[0]
spec <- spec.arma(ar=ar, ma=0)

d <- .02
s <- spectrum(logrets_past, span = 50, log="no", taper = 0, detrend = FALSE,
              plot=F)
sx <- s$freq/d
sy <- 2*s$spec
plot(sy~sx,xlab="frequency",ylab="Spectral density",type="l", main="Smoothed Periodogram")


NFLX <- as.ts(NFLX_past)
resSpec <- spec.mtm(NFLX, k=10, nw=10, nFFT = "default", Ftest = TRUE, log="yes") 
