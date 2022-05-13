library(tidyverse)
library(tseries)
library(forecast)
library(MTS)
library(lmtest)

##############################
# Reading in / cleaning data #
##############################

# Monthly CPI (index 1982-1984=100)
CPI = read.csv(file.choose()) %>% rename(CPI = CPIAUCNS)

# Monthly M2 money supply (billions of dollars)
M2 = read.csv(file.choose()) %>% rename(M2 = M2NS)

# join and convert to time series
y = left_join(CPI, M2, by="DATE") %>% select(-DATE)
y$CPI = ts(y$CPI, frequency=12, start=c(2012, 1))
y$M2 = ts(y$M2, frequency=12, start=c(2012, 1))
plot.ts(y)

#Plotting both time series
plot.ts(y$CPI, main = "Consumer Price Index For All Urban Consumers", ylab = "CPIAUC", col = "red")
plot.ts(y$M2, main = "M2 Money Supply", ylab = "M2 (Billions)", col = "blue")

#Mean and standard deviation of CPI ts
mean(y$CPI)
sd(y$CPI)

#Autocorrelation Plot
acf(y$CPI, type = "correlation", main = "Autocorrelation of CPIAUC", lag.max = 40) 

#Additive or Multiplicative 
#ADD
y_decomp = decompose(y$CPI, type = "additive")


#MULT
y_decomp_mult = decompose(y$CPI, type = "multiplicative")

plot(y_decomp_mult$trend)
plot.ts(y_decomp_mult$seasonal[1:12])


#MAPE
mape = function(pred,true){
  return(mean ( abs( (pred - true) / true ) ,na.rm=T))
}


y_pred_add = y_decomp$trend + y_decomp$seasonal
y_pred_mult = y_decomp_mult$trend * y_decomp_mult$seasonal 

print(mape(y_pred_add, y$CPI))
print(mape(y_pred_mult, y$CPI))

#Trend and Seasonality in Additive Plot
plot.ts(y_decomp$trend, main = "Additive Trend Plot (CPI)", ylab = 'Trend',col = 'red')
plot.ts(y_decomp$seasonal[1:12], main = "Additive Seasonality Plot (CPI)", ylab = 'Seasonal Effect', col = 'blue')

################
# Examining M2 #
################

# Mean and standard deviation of M2 ts
mean(y$M2)
sd(y$M2)

#Autocorrelation Plot
acf(y$M2, type = "correlation", main = "Autocorrelation of M2", lag.max = 40) 

#Additive or Multiplicative 
#ADD
y_decomp = decompose(y$M2, type = "additive")


#MULT
y_decomp_mult = decompose(y$M2, type = "multiplicative")

plot(y_decomp_mult$trend)
plot.ts(y_decomp_mult$seasonal[1:12])

y_pred_add = y_decomp$trend + y_decomp$seasonal
y_pred_mult = y_decomp_mult$trend * y_decomp_mult$seasonal 

print(mape(y_pred_add, y$M2))
print(mape(y_pred_mult, y$M2))

#Trend and Seasonality in Additive Plot
plot.ts(y_decomp$trend, main = "Additive Trend Plot (M2)", ylab = 'Trend',col = 'red')
plot.ts(y_decomp$seasonal[1:12], main = "Additive Seasonality Plot (M2)", ylab = 'Seasonal Effect', col = 'blue')

#########################
# Stationarity Analysis #
#########################

# Testing for stationarity
adf.test(y$CPI); kpss.test(y$CPI, null="Trend")
adf.test(y$M2); kpss.test(y$M2, null="Trend")

# Adjusting for seasonality
cpi_smoothed = HoltWinters(y$CPI, alpha=0.1, beta=FALSE, gamma=FALSE, seasonal="additive")
cpi_smoothed = cpi_smoothed$fitted[,"xhat"]
M2_smoothed = HoltWinters(y$M2, alpha=0.1, beta=FALSE, gamma=FALSE, seasonal="additive")
M2_smoothed = M2_smoothed$fitted[,"xhat"]
y_smoothed = data.frame(cpi_smoothed, M2_smoothed) %>% rename(CPI = cpi_smoothed, M2 = M2_smoothed)

for (i in ncol(y)){
  par(mfrow=c(2,1))
  acf(y[,i]); pacf(y[,i])
  
  print(ndiffs(y[,i], test="adf"))
  print(ndiffs(y[,i], test="kpss"))
}

par(mfrow=c(2,2))
plot.ts(diff(y$cpi)); plot.ts(diff(log(y$cpi)))
plot.ts(diff(y$M2)); plot.ts(diff(log(y$M2)))

par(mfrow=c(2,1))
acf(diff(y$cpi)); pacf(diff(log(y$cpi)))
acf(diff(y$M2)); pacf(diff(log(y$M2)))

######################
# Forecasting models #
######################

# Training/test split (80%/20%)
i_train = ceiling(0.80*nrow(CPI))
CPI$DATE[i_train]

# CPI
cpi_train = window(y$CPI, end=c(2020,3))
cpi_test = window(y$CPI, start=c(2020,4))
plot.ts(cpi_train, main="CPI Test/Train Split", ylab="CPI", xlim=c(2012, 2022), ylim=c(230,290))
lines(cpi_test, col="red")
legend(x="topleft", legend=c("train", "test"), col=c("black", "red"))

# M2
# M2_train = window(y$M2, end=c(2020,3))
# M2_test = window(y$M2, start=c(2020,4))
# plot.ts(M2_train, main="M2 Test/Train Split", ylab="M2", xlim=c(2012, 2022), ylim=c(10000, 22000))
# lines(M2_test, col="red")
# legend(x="topleft", legend=c("train", "test"), col=c("black", "red"))

##### ARIMA model
arima.model = auto.arima(cpi_train, max.d = 12, max.p = 12, max.q = 12,
                   max.D = 12, max.P = 12, max.Q = 12,
                   allowdrift=TRUE)
arima.pred = arima.model$fitted
arimaorder(arima.model)
mape(arima.pred, cpi_train)

arima.fc = forecast(arima.model, h=length(cpi_test))$mean
plot.ts(cpi_train, xlim=c(2012, 2022), ylim=c(230,290), main="ARIMA-Forecasted CPI", ylab="CPI")
lines(cpi_test, col="red")
lines(arima.fc, col="blue")
legend(x="topleft", legend=c("train", "test", "forecast"), col=c("black", "red", "blue"))
mape(arima.fc, cpi_test)

##### VARMA model
ndiffs(y$M2)
y.diff = data.frame(apply(y, MARGIN=2, FUN=function(y){diff(diff(y))}))
y.diff = ts(y.diff, start=c(2012, 1), frequency=12)
kpss.test(y.diff[,"CPI"])

grangertest(CPI ~ M2, data=y.diff, order=12) # p < 0.05 --- M2 needed to predict CPI
grangertest(M2 ~ CPI, data=y.diff, order=12) # p > 0.05 --- CPI not needed to predict M2

y.train = window(y.diff, end=c(2020,3))
y.test = window(y.diff, start=c(2020,4))

Eccm(y.train, maxp=12, maxq=12) # try VARMA(5,0) and VARMA(2,1)
var1 = VARMA(y.train, p=5, q=0, include.mean=FALSE)
var2 = VARMA(y.train, p=2, q=1, include.mean=FALSE)
var1$aic; var2$aic # VARMA(5,0) is sufficient

constrained.var = refVARMA(var1, thres=2)
constrained.var$Phi

constrained.var$aic

y.pred = VARMApred(constrained.var, h=nrow(y.test))$pred

y.true = rbind(y[1,], y.train, y.test)
y.pred = rbind(y[1,], y.train, y.pred)

y.true = apply(y.true, MARGIN=2, FUN=cumsum)
y.pred = apply(y.pred, MARGIN=2, FUN=cumsum)

y.true = ts(y.true, start=c(2012, 1), frequency=12)
y.pred = ts(y.pred, start=c(2012, 1), frequency=12)

cpi.pred = y.pred[, "CPI"]
cpi.pred.train = window(cpi.pred, end=c(2020,3))
cpi.pred.test = window(cpi.pred, start=c(2020,4))
plot.ts(cpi_train, main="VARMA-predicted CPI", xlim=c(2012, 2022), ylim=c(220,290))
lines(cpi.pred.train, col="green")
lines(cpi_test, col="blue")
lines(cpi.pred.test, col="red")
legend(x="topleft", legend=c("train", "test", "fc.train", "fc.test"), col=c("black", "red", "blue"))

mape(cpi.pred.train, cpi_train)
mape(cpi.pred.test, cpi_test)








