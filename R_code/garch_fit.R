library(fGarch)
data <- logrets_past
n = length(data)

plot(logrets_past, main = "NFLX Log-Returns")


pacf(logrets_past, lag.max = 200, main = paste("ACF of ", expression(X)))
acf(logrets_past^2, lag.max = 200)
pacf(logrets_past^2, lag.max = 200)



########
## Fits
########

best_fit_AIC <- garchAuto(data, cond.dists = "sged", trace = T, ic = "AIC")
plot(best_fit_AIC)
summary(best_fit_AIC)
predict(best_fit_AIC, n.ahead = 23, plot = T)
texreg(sg)

fit1_aic <- garchFit(~arma(2,0) + garch(1, 1), data = data, trace=F, cond.dist = "sged", include.mean = F)
summary(fit1_aic)
plot(fit1_aic, which = 13)
predict(fit1_aic, plot=T, n.ahead = 61)

fit2_aic <- garchFit(~arma(0,5) + garch(2, 1), data = data, trace=F, cond.dist = "sged", include.mean = F)
summary(fit2_aic)
plot(fit2_aic, which = 13)
predict(fit2_aic, plot=T, n.ahead = 50)

plot(c(as.numeric(logrets_past),as.numeric(logrets_pres)),type='l',
     ylab=expression(X[t]))
legend('topleft', legend=c(expression(X[t]), "Fitted/Forecast", "UpperCI", "LowerCI"),
       col = c("black", "red","green","blue"),lty=1)
lines(c(best_fit_AIC@fitted, forecast_garchA1_past$meanForecast),col='red')
lines(x=c(459:481),forecast_garchA1_past$lowerInterval,col='blue')
lines(x=c(459:481),forecast_garchA1_past$upperInterval,col='green')
polygon(c(c(459:481), rev(c(459:481))), 
        c(forecast_garchA1_past$upperInterval, rev(forecast_garchA1_past$lowerInterval)),
        col = rgb(0,0,0,.2), border = NA)

rmse(actual = logrets_past, predicted = best_fit_AIC@fitted)
mae(actual = logrets_past, predicted = best_fit_AIC@fitted)
mpe(actual = logrets_past, predicted = best_fit_AIC@fitted)
mape(actual = logrets_past, predicted = best_fit_AIC@fitted)
mase(actual = logrets_past, predicted = best_fit_AIC@fitted)
####
## Diagnostics 
####
Box.test(best_fit_AIC@residuals,lag=4, type = "Ljung") ## fails indep -> expected as serial cor with garch
plot(residuals(best_fit_AIC) ~ fitted(best_fit_AIC))
plot(sqrt(abs(residuals(best_fit_AIC))) ~ fitted(best_fit_AIC)) # indicates NcV


########
## Forecast accuracy
########

# fit1_AIC
forecast_garchA1_past <- predict(best_fit_AIC, n.ahead = 23, plot=T)

lines(logrets_pres)

# y <- as.numeric(NFLX$NFLX.Close)
# x <- index(NFLX)
# pp <- vector(length=25)
# pp[1] <- NFLX_past$NFLX.Close[459]
# for (j in 2:25){
#   pp[j] <- exp(forecast_garchA1_past$meanForecast[j])*pp[j-1]
# }
# 
# p <- cbind(index(NFLX_pres[1:59]),pp[2:60])
# cl <- p+qsged(.025, nu=1, xi=fit1_bic@fit$par['skew'])*forecast_garchA1_past$meanError
# cu <- p+qsged(.975, nu=1, xi=fit1_bic@fit$par['skew'])*forecast_garchA1_past$meanError
# 
# lower <- forecast_garchA1_past$meanForecast + forecast_garchA1_past$lowerInterval
# upper <- forecast_garchA1_past$meanForecast + forecast_garchA1_past$upperInterval
# 
# plot(as.numeric(NFLX$NFLX.Close[340:419]), ylim = range(c(NFLX$NFLX.Close[340:419]), p[,2]), type='l')
# 
# plot(x,NFLX$NFLX.Close, xlim = range(c(x[340:419],index(NFLX_pres[1:61]))), ylim=range(y),type='l')
# plot(x=as.Date(p[,1]), y = p[,2], col='red',type='l')
# lines(lower, col='blue',type='l')
# lines(cu, col='green',type='l')

test_res_gA1 <- as.numeric(forecast_garchA1_past$meanForecast) - logrets_pres[1:23]
plot(test_res_gA1)


acc_gA1 <- DACTest(forecast_garchA1_past[,1], actual = logrets_pres[1:23], test = "PT")


rmse_gA1 <- rmse(predicted = as.numeric(forecast_garchA1_past$meanForecast), actual = logrets_pres)
mae_gA1 <- mae(predicted = as.numeric(forecast_garchA1_past$meanForecast), actual = logrets_pres)
mape_gA1 <- smape(predicted = as.numeric(forecast_garchA1_past$meanForecast), actual = logrets_pres)
sse_gA1 <- sse(predicted = as.numeric(forecast_garchA1_past$meanForecast), actual = logrets_pres)
(accA1 <- data.frame(SSE = sse_gA1, RMSE = rmse_gA1, MAE = mae_gA1, SMAPE = mape_gA1))

mape(predicted = as.numeric(forecast_garchA1_past$meanForecast), actual = logrets_pres)

########
## SPEC
########

del<-0.01 # sampling interval
x.spec <- spectrum(best_fit_AIC@residuals^2,log="no",span=100,plot=F)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")




NFLX_Growth_rate <- as.ts(logrets_past)
resSpec <- spec.mtm(NFLX_Growth_rate, k=10, nw=20, nFFT = "default", Ftest = TRUE, log="yes") 
