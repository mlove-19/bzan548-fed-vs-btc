library(tidyverse)
library(lubridate)
library(janitor)
library(tseries)
library(forecast)

##############################
# Reading in / cleaning data #
##############################

setwd("C:/MSBA/BZAN 548/bzan548-fed-vs-btc/datasets")

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

cpi1 = window(cpi, start=c(2020,1)); m2.1 = window(m_2, start=c(2020,1))
par(mfrow=c(2,1))
plot.ts(cpi1); plot.ts(m2.1)
