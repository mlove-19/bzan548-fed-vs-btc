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
y$cpi = ts(y$cpi, frequency=12, start=c(2012, 1))
y$M2 = ts(y$M2, frequency=12, start=c(2012, 1))
plot.ts(y)

# Testing for stationarity
adf.test(y$cpi); kpss.test(y$cpi)
adf.test(y$M2); kpss.test(y$M2)

# Adjusting for seasonality
cpi_smoothed = HoltWinters(y$cpi, alpha=0.1, beta=FALSE, gamma=FALSE)
cpi_smoothed = cpi_smoothed$fitted[,"xhat"]
M2_smoothed = HoltWinters(y$M2, alpha=0.1, beta=FALSE, gamma=FALSE)
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



