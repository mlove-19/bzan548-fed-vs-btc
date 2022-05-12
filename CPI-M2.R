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

############################
# Stationarity Adjustments #
############################

# Testing for stationarity
adf.test(y$CPI); kpss.test(y$CPI, null="Trend")
adf.test(y$M2); kpss.test(y$M2, null="Trend")

# Adjusting for seasonality
cpi_smoothed = HoltWinters(y$CPI, alpha=0.1, beta=FALSE, gamma=FALSE, seasonal="additive")
cpi_smoothed = cpi_smoothed$fitted[,"xhat"]
M2_smoothed = HoltWinters(y$M2, alpha=0.1, beta=FALSE, gamma=FALSE, seasonal="additive")
M2_smoothed = M2_smoothed$fitted[,"xhat"]

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

y_diff = data.frame(apply(y, MARGIN=2, FUN=diff))

grangertest(cpi ~ M2, data=y_diff) # p < 0.05 --- M2 needed to predict CPI
grangertest(M2 ~ cpi, data=y_diff) # p > 0.05 --- CPI not needed to predict M2


# plot.ts(y$CPI)
# lines(cpi_smoothed, col="red")
# 
# plot.ts(y$M2)
# lines(M2_smoothed, col="red")
