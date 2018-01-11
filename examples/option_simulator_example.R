setwd('examples')
rm(list = ls())

library('akima')
library('RQuantLib')

source('../R/simulateOptionVectors.R')

load('option_sample_data.Rdata')

in_type <- 'put'

in_underlying <-  'SPX'

in_holding_period <- 28


startDate <- as.Date('2015-01-01')

vol_data <- vol[as.Date(datestamp) >= startDate & underlying == in_underlying]

price_data <- price[as.Date(datestamp) >= startDate & underlying == in_underlying]

ts_simulated_options <- simulateOptionVectors(
        vol_data = vol_data,
        price_data = price_data,
        in_underlying = in_underlying,
        in_type = in_type,
        in_holding_period = in_holding_period)
