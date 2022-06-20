library(tidyverse)
library(nets)
library(forecast)
library(xtable)

# Option 1: Mean
data = read_csv('Idiosyncratic Inflation (Factoring out mean inflation).csv')  %>%
  select(-c('Japan', 'Australia', 'Korea, Rep. of', 'Indonesia', 'New Zealand', 'Romania'))

results1 = data.frame(country = character(), mse_ar1 = numeric(), mse_nets = numeric())

# AR(1)
for (c in 2:42) {
  
  country = colnames(data)[c]
  timeseries <- ts(data[1:115,c], start=c(1992, 2), frequency = 4)
  fit = Arima(timeseries, order=c(1,0,0))
  pred = forecast(fit, h = 4)$mean
  mse = sum((as.matrix(data[116:119,c]) - as.matrix(pred))^2)
  
  results1[c-1, 1] = colnames(data)[c]
  results1[c-1, 2] = mse/4
  
}

# Nets
model = nets(y=as.matrix(data[1:115,2:42]),
             lambda = 0.01)

pred = predict(model, newdata = as.matrix(data[116:119,2:42]))$y.hat
mse = colSums((as.matrix(data[116:119,2:42]) - pred)^2)

results1$mse_nets <- mse/4

results1 = results1 %>%
  mutate(net_wins = ifelse(mse_nets < mse_ar1,
                           yes = 1,
                           no = 0))

write_csv(results1, 'mse_results_factor_mean.csv')

# Option 2: GDP growth
data = read_csv('Idiosyncratic Inflation (Factoring out GDP growth).csv') %>%
  select(-c('Japan', 'Australia', 'Korea, Rep. of', 'Indonesia', 'New Zealand', 'Romania'))
results2 = data.frame(country = character(), mse_ar1 = numeric(), mse_nets = numeric())

# AR(1)
for (c in 2:32) {
  
  country = colnames(data)[c]
  timeseries <- ts(data[1:62,c], start=2005, frequency = 4)
  fit = Arima(timeseries, order=c(1,0,0))
  pred = forecast(fit, h = 4)$mean
  mse = sum((as.matrix(data[63:66,c]) - as.matrix(pred))^2)
  
  results2[c-1, 1] = colnames(data)[c]
  results2[c-1, 2] = mse/4
  
}

# Nets
model = nets(y=as.matrix(data[1:62,2:32]),
             lambda = 0.01)

pred = predict(model, newdata = as.matrix(data[63:66,2:32]))$y.hat
mse = colSums((as.matrix(data[63:66,2:32]) - pred)^2)

results2$mse_nets <- mse/4

results2 = results2 %>%
  mutate(net_wins = ifelse(mse_nets < mse_ar1,
                           yes = 1,
                           no = 0))

write_csv(results2, 'mse_results_factor_gdp.csv')


# Join tables

results1 %>%
  full_join(results2, by="country") %>%
  mutate_if(is.numeric, sqrt) %>%
  xtable(include.rownames=FALSE) %>%
  print(include.rownames=FALSE)
