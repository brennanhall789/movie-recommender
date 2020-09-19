coef(best_fit_AIC)

ar_coef_AIC <- coef(best_fit_AIC)[c(1:2)]
ma_coef_AIC <- coef(best_fit_AIC)[c(3:6)]
g_coef_AIC <- coef(best_fit_AIC)[c(8:9)]

par(mfrow=c(1,2))
## Stationarity of ARMA (passes)
ARroots_AIC <- polyroot(c(1,-ar_coef_AIC))
plot.roots(ma.roots = ARroots_AIC, main = "ARMA(2,4) + GARCH(1,1) AR roots")

## Invertibility of ARMA (passes)
MAroots_AIC <- polyroot(c(1,ma_coef_AIC))
plot.roots(ma.roots = MAroots_AIC, main = "MA roots")

## Stationarity of GARCH (passes)
sum(g_coef_AIC) < 1



ar_coef_BIC <- best_fit_BIC@fit$coef[c(NULL)]
ma_coef_BIC <- best_fit_BIC@fit$coef[c(2)]

## Stationarity of ARMA (passes)
ARroots_BIC <- polyroot(c(1,-ar_coef_BIC))
plot.roots(ma.roots = ARroots_BIC, main = "BIC Model AR roots")

## Invertibility of ARMA (passes)
MAroots_BIC <- polyroot(c(1,ma_coef_BIC))
plot.roots(ma.roots = MAroots_BIC, main = "BIC model MA roots")

## Stationarity of GARCH (passes)
sum(best_fit_BIC@fit$coef[c(3:4)]) < 1

