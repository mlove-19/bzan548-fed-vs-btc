library(tidyverse)
library(lubridate)
library(janitor)
library(tseries)
library(forecast)

##############################
# Reading in / cleaning data #
##############################

setwd("/home/mlove/MSBA/BZAN 548 Time Series Forecasting/Final project")

# Monthly BTC trading volume (USD)
btc = read.csv("bitcoinity_data.csv") %>% 
  transmute(volume = select(., bit.x:others) %>% rowSums(na.rm=TRUE)) %>% 
  ts(frequency=12, start=c(2012, 1))

# Monthly CPI (index 1982-1984=100)
cpi = read.csv("CPIAUCNS.csv") %>% 
  rename(cpi = CPIAUCNS) %>% 
  select(cpi) %>% 
  ts(frequency=12, start=c(2012, 1))

# Monthly M2 money supply (billions of dollars)
m_2 = read.csv("M2NS.csv") %>% 
  rename(M2 = M2NS) %>% 
  select(M2) %>% 
  ts(frequency=12, start=c(2012, 1))

plot.ts(btc); plot.ts(cpi); plot.ts(m_2)
