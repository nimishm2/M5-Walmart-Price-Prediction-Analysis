## Data Preparation-------------------------------------------------------------

# load datasets
d1 = "/Users/alexasisi/Downloads/econ491 data/sell_prices.csv"
d2 = "/Users/alexasisi/Downloads/econ491 data/calendar.csv"
d3 = "/Users/alexasisi/Downloads/econ491 data/sales_train_validation.csv" #train
d4 = "/Users/alexasisi/Downloads/econ491 data/sales_test_validation.csv" #test
d5 = "/Users/alexasisi/Downloads/econ491 data/weights_validation.csv"
d6 = "/Users/alexasisi/Downloads/econ491 data/sales_test_evaluation.csv" #actual sales

prices = read.csv(d1)
calendar = read.csv(d2)
sales1 = read.csv(d3)
sales2 = read.csv(d4)
w = read.csv(d5)
actual_sales = read.csv(d6)


# load packages
library(tidyverse)
library(magrittr)
library(gridExtra)
library(plotly)
library(officer)
library(rvg)

# Library for ARIMA forecasting
library(forecast)
library(fable)
library(MuMIn)
library(tseries)

# aggregate calendar
calendar %>% filter(!is.na(event_name_1)) %>%
  head(1) %>%
  select(date, weekday, event_name_1, event_type_1, event_name_2, event_type_2)

## Merge Sales -----------------------------------------------------------------

# combine sales datasets
sales<-sales1 %>%
  left_join(sales2, by = c(
    "item_id", "dept_id", "cat_id", "store_id", "state_id"
  ))

# pivot
sales %<>% pivot_longer(cols = all_of(
  colnames(sales)[str_detect(colnames(sales), "d_")]
), names_to = "day_label", values_to = "units")

# aggregate to category/day
sales %<>% group_by(cat_id, day_label, state_id) %>% 
  summarise(units = sum(units)) %>% 
  ungroup()

date_dict = tibble("date" = seq.Date(from = as.Date("2011-01-29"),
                                     to = as.Date("2016-06-19"), by = 1))

date_dict$day_label = paste0("d_", 1:nrow(date_dict))

# joining and sorting
sales %<>% left_join(date_dict, by = "day_label") %>% 
  select(-day_label) %>% 
  group_by(cat_id) %>%
  arrange(date, .by = T) %>% 
  ungroup()

# reorder the columns
sales %<>% select(cat_id, date, units, state_id)


## Merge Actual Sales ----------------------------------------------------------

# pivot
actual_sales %<>% pivot_longer(cols = all_of(
  colnames(actual_sales)[str_detect(colnames(actual_sales), "d_")]
), names_to = "day_label", values_to = "units")

# aggregate to category/day
actual_sales %<>% group_by(cat_id, day_label, state_id) %>% 
  summarise(units = sum(units)) %>% 
  ungroup()

# joining and sorting
actual_sales %<>% left_join(date_dict, by = "day_label") %>% 
  select(-day_label) %>% 
  group_by(cat_id) %>%
  arrange(date, .by = T) %>% 
  ungroup()

# reorder the columns
actual_sales %<>% select(cat_id, date, units, state_id)

# date dictionary from 2016-05-23 to 2016-06-19
date_dict_forecast = tibble("date" = seq.Date(from = as.Date("2016-05-23"),
                                              to = as.Date("2016-06-19"), by = 1))


## ARIMA.......................................................................

# Split data by category and state

# sales
sales_split <- sales %>%
  filter(cat_id %in% c("FOODS", "HOBBIES", "HOUSEHOLD")) %>%
  group_by(cat_id, state_id) %>%
  nest()

# actual sales
actual_sales_split <- actual_sales %>%
  filter(cat_id %in% c("FOODS", "HOBBIES", "HOUSEHOLD")) %>%
  group_by(cat_id, state_id) %>%
  nest()

view(sales)
view(actual_sales)
view(sales_split)
view(actual_sales_split)

# Category: FOODS, State: California, Code: FCA +++

sales_split$data[[1]]
actual_sales_split$data[[1]]

# To check stationery

adf.test(sales_split$data[[1]]$units)

# Differencing

plot(sales_split$data[[1]]$units)

diff_data_1 <- diff (sales_split$data[[1]]$units, differences = 1)  # 1st differencing
plot(diff_data_1)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_1)
pacf(diff_data_1)

# Fit the ARIMA Model

seasonal_fit_1_versionA <- arima(sales_split$data[[1]]$units, 
                          order=c(5,0,1), 
                          seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_1_versionB <- arima(sales_split$data[[1]]$units, 
                          order=c(5,0,1), 
                          seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_1_stepwise <-auto.arima(sales_split$data[[1]]$units, seasonal = TRUE)
seasonal_fit_1_search <-auto.arima(sales_split$data[[1]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_1_versionA, seasonal_fit_1_versionB)

# Validate the Model

checkresiduals(seasonal_fit_1_versionA)

# Forecast

forecasted_results_1 <- forecast(seasonal_fit_1_versionA, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_1 <- data.frame(forecasted_results_1)
actual_values_1 <- actual_sales_split$data[[1]]$units
accuracy(forecasted_values_1$Point.Forecast, actual_values_1)

# Visualize The Forecast

# Binding the date with values
forecasted_values_1_with_date <- cbind(date_dict_forecast,forecasted_values_1$Point.Forecast)
actual_values_1_with_date <- cbind(date_dict_forecast,actual_values_1)

# Create date character
forecasted_values_1_with_date$date_char <- as.character(forecasted_values_1_with_date$date)
actual_values_1_with_date$date_char <- as.character(actual_values_1_with_date$date)

# Create the graphs with hover label
graphs_1 <- plot_ly(data = forecasted_values_1_with_date, x = forecasted_values_1_with_date$date, y = forecasted_values_1_with_date$`forecasted_values_1$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_1_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_1 <- graphs_1 %>% add_trace (data = actual_values_1_with_date, x = actual_values_1_with_date$date, y = actual_values_1_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_1_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_1 <- graphs_1 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_1

# ++++

# Category: FOODS, State: Texas, Code: FTX +++

sales_split$data[[2]]
actual_sales_split$data[[2]]

# To check stationery

adf.test(sales_split$data[[2]]$units)

# Differencing

plot(sales_split$data[[2]]$units)

diff_data_2 <- diff (sales_split$data[[2]]$units, differences = 1)  # 1st differencing
plot(diff_data_2)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_2)
pacf(diff_data_2)

# Fit the ARIMA Model

seasonal_fit_2_versionA <- arima(sales_split$data[[2]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_2_versionB <- arima(sales_split$data[[2]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_2_stepwise <-auto.arima(sales_split$data[[2]]$units, seasonal = TRUE)
seasonal_fit_2_search <-auto.arima(sales_split$data[[2]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_2_versionA, seasonal_fit_2_versionB)

# Validate the Model

checkresiduals(seasonal_fit_2_versionA)

# Forecast

forecasted_results_2 <- forecast(seasonal_fit_2_versionA, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_2 <- data.frame(forecasted_results_2)
actual_values_2 <- actual_sales_split$data[[2]]$units
accuracy(forecasted_values_2$Point.Forecast, actual_values_2)

# Visualize The Forecast

# Binding the date with values
forecasted_values_2_with_date <- cbind(date_dict_forecast,forecasted_values_2$Point.Forecast)
actual_values_2_with_date <- cbind(date_dict_forecast,actual_values_2)

# Create date character
forecasted_values_2_with_date$date_char <- as.character(forecasted_values_2_with_date$date)
actual_values_2_with_date$date_char <- as.character(actual_values_2_with_date$date)

# Create the graphs with hover label
graphs_2 <- plot_ly(data = forecasted_values_2_with_date, x = forecasted_values_2_with_date$date, y = forecasted_values_2_with_date$`forecasted_values_2$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_2_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_2 <- graphs_2 %>% add_trace (data = actual_values_2_with_date, x = actual_values_2_with_date$date, y = actual_values_2_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_2_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_2 <- graphs_2 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_2

# ++++

# Category: FOODS, State: Wisconsin, Code: FWI +++

sales_split$data[[3]]
actual_sales_split$data[[3]]

# To check stationery

adf.test(sales_split$data[[3]]$units)

# Differencing

plot(sales_split$data[[3]]$units)

diff_data_3 <- diff (sales_split$data[[3]]$units, differences = 1)  # 1st differencing
plot(diff_data_3)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_3)
pacf(diff_data_3)

# Fit the ARIMA Model

seasonal_fit_3_versionA <- arima(sales_split$data[[3]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_3_versionB <- arima(sales_split$data[[3]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_3_stepwise <-auto.arima(sales_split$data[[3]]$units, seasonal = TRUE)
seasonal_fit_3_search <-auto.arima(sales_split$data[[3]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_3_versionA, seasonal_fit_3_versionB)

# Validate the Model

checkresiduals(seasonal_fit_3_versionA)

# Forecast

forecasted_results_3 <- forecast(seasonal_fit_3_versionA, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_3 <- data.frame(forecasted_results_3)
actual_values_3 <- actual_sales_split$data[[3]]$units
accuracy(forecasted_values_3$Point.Forecast, actual_values_3)

# Visualize The Forecast

# Binding the date with values
forecasted_values_3_with_date <- cbind(date_dict_forecast,forecasted_values_3$Point.Forecast)
actual_values_3_with_date <- cbind(date_dict_forecast,actual_values_3)

# Create date character
forecasted_values_3_with_date$date_char <- as.character(forecasted_values_3_with_date$date)
actual_values_3_with_date$date_char <- as.character(actual_values_3_with_date$date)

# Create the graphs with hover label
graphs_3 <- plot_ly(data = forecasted_values_3_with_date, x = forecasted_values_3_with_date$date, y = forecasted_values_3_with_date$`forecasted_values_3$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_3_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_3 <- graphs_3 %>% add_trace (data = actual_values_3_with_date, x = actual_values_3_with_date$date, y = actual_values_3_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_3_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_3 <- graphs_3 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_3

# ++++

# Category: Hobbies, State: California, Code: HCA +++

sales_split$data[[4]]
actual_sales_split$data[[4]]

# To check stationery

adf.test(sales_split$data[[4]]$units)

# Differencing

plot(sales_split$data[[4]]$units)

diff_data_4 <- diff (sales_split$data[[4]]$units, differences = 1)  # 1st differencing
plot(diff_data_4)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_4)
pacf(diff_data_4)

# Fit the ARIMA Model

seasonal_fit_4_versionA <- arima(sales_split$data[[4]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_4_versionB <- arima(sales_split$data[[4]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_4_stepwise <-auto.arima(sales_split$data[[4]]$units, seasonal = TRUE)
seasonal_fit_4_search <-auto.arima(sales_split$data[[4]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_4_versionA, seasonal_fit_4_versionB)

# Validate the Model

checkresiduals(seasonal_fit_4_versionA)

# Forecast

forecasted_results_4 <- forecast(seasonal_fit_4_versionA, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_4 <- data.frame(forecasted_results_4)
actual_values_4 <- actual_sales_split$data[[4]]$units
accuracy(forecasted_values_4$Point.Forecast, actual_values_4)

# Visualize The Forecast

# Binding the date with values
forecasted_values_4_with_date <- cbind(date_dict_forecast,forecasted_values_4$Point.Forecast)
actual_values_4_with_date <- cbind(date_dict_forecast,actual_values_4)

# Create date character
forecasted_values_4_with_date$date_char <- as.character(forecasted_values_4_with_date$date)
actual_values_4_with_date$date_char <- as.character(actual_values_4_with_date$date)

# Create the graphs with hover label
graphs_4 <- plot_ly(data = forecasted_values_4_with_date, x = forecasted_values_4_with_date$date, y = forecasted_values_4_with_date$`forecasted_values_4$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_4_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_4 <- graphs_4 %>% add_trace (data = actual_values_4_with_date, x = actual_values_4_with_date$date, y = actual_values_4_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_4_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_4 <- graphs_4 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_4

# ++++

# Category: Hobiies, State: Texas, Code: HTX +++

sales_split$data[[5]]
actual_sales_split$data[[5]]

# To check stationery

adf.test(sales_split$data[[5]]$units)

# Differencing

plot(sales_split$data[[5]]$units)

diff_data_5 <- diff (sales_split$data[[5]]$units, differences = 1)  # 1st differencing
plot(diff_data_5)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_5)
pacf(diff_data_5)

# Fit the ARIMA Model

seasonal_fit_5_versionA <- arima(sales_split$data[[5]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_5_versionB <- arima(sales_split$data[[5]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_5_stepwise <-auto.arima(sales_split$data[[5]]$units, seasonal = TRUE)
seasonal_fit_5_search <-auto.arima(sales_split$data[[5]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_5_versionA, seasonal_fit_5_versionB)

# Validate the Model

checkresiduals(seasonal_fit_5_versionB)

# Forecast

forecasted_results_5 <- forecast(seasonal_fit_5_versionB, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_5 <- data.frame(forecasted_results_5)
actual_values_5 <- actual_sales_split$data[[5]]$units
accuracy(forecasted_values_5$Point.Forecast, actual_values_5)

# Visualize The Forecast

# Binding the date with values
forecasted_values_5_with_date <- cbind(date_dict_forecast,forecasted_values_5$Point.Forecast)
actual_values_5_with_date <- cbind(date_dict_forecast,actual_values_5)

# Create date character
forecasted_values_5_with_date$date_char <- as.character(forecasted_values_5_with_date$date)
actual_values_5_with_date$date_char <- as.character(actual_values_5_with_date$date)

# Create the graphs with hover label
graphs_5 <- plot_ly(data = forecasted_values_5_with_date, x = forecasted_values_5_with_date$date, y = forecasted_values_5_with_date$`forecasted_values_5$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_5_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_5 <- graphs_5 %>% add_trace (data = actual_values_5_with_date, x = actual_values_5_with_date$date, y = actual_values_5_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_5_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_5 <- graphs_5 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_5

# ++++

# Category: Hobbies, State: Wisconsin, Code: HWI +++

sales_split$data[[6]]
actual_sales_split$data[[6]]

# To check stationery

adf.test(sales_split$data[[6]]$units)

# Differencing

plot(sales_split$data[[6]]$units)

diff_data_6 <- diff (sales_split$data[[6]]$units, differences = 1)  # 1st differencing
plot(diff_data_6)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_6)
pacf(diff_data_6)

# Fit the ARIMA Model

seasonal_fit_6_versionA <- arima(sales_split$data[[6]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_6_versionB <- arima(sales_split$data[[6]]$units, 
                                 order=c(4,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_6_stepwise <-auto.arima(sales_split$data[[6]]$units, seasonal = TRUE)
seasonal_fit_6_search <-auto.arima(sales_split$data[[6]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_6_versionA, seasonal_fit_6_versionB)

# Validate the Model

checkresiduals(seasonal_fit_6_versionB)

# Forecast

forecasted_results_6 <- forecast(seasonal_fit_6_versionB, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_6 <- data.frame(forecasted_results_6)
actual_values_6 <- actual_sales_split$data[[6]]$units
accuracy(forecasted_values_6$Point.Forecast, actual_values_6)

# Visualize The Forecast

# Binding the date with values
forecasted_values_6_with_date <- cbind(date_dict_forecast,forecasted_values_6$Point.Forecast)
actual_values_6_with_date <- cbind(date_dict_forecast,actual_values_6)

# Create date character
forecasted_values_6_with_date$date_char <- as.character(forecasted_values_6_with_date$date)
actual_values_6_with_date$date_char <- as.character(actual_values_6_with_date$date)

# Create the graphs with hover label
graphs_6 <- plot_ly(data = forecasted_values_6_with_date, x = forecasted_values_6_with_date$date, y = forecasted_values_6_with_date$`forecasted_values_6$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_6_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_6 <- graphs_6 %>% add_trace (data = actual_values_6_with_date, x = actual_values_6_with_date$date, y = actual_values_6_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_6_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_6 <- graphs_6 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_6

# ++++

# Category: Household, State: California, Code: HHCA +++

sales_split$data[[7]]
actual_sales_split$data[[7]]

# To check stationery

adf.test(sales_split$data[[7]]$units)

# Differencing

plot(sales_split$data[[7]]$units)

diff_data_7 <- diff (sales_split$data[[7]]$units, differences = 1)  # 1st differencing
plot(diff_data_7)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_7)
pacf(diff_data_7)

# Fit the ARIMA Model

seasonal_fit_7_versionA <- arima(sales_split$data[[7]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_7_versionB <- arima(sales_split$data[[7]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_7_stepwise <-auto.arima(sales_split$data[[7]]$units, seasonal = TRUE)
seasonal_fit_7_search <-auto.arima(sales_split$data[[7]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_7_versionA, seasonal_fit_7_versionB)

# Validate the Model

checkresiduals(seasonal_fit_7_versionA)

# Forecast

forecasted_results_7 <- forecast(seasonal_fit_7_versionA, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_7 <- data.frame(forecasted_results_7)
actual_values_7 <- actual_sales_split$data[[7]]$units
accuracy(forecasted_values_7$Point.Forecast, actual_values_7)

# Visualize The Forecast

# Binding the date with values
forecasted_values_7_with_date <- cbind(date_dict_forecast,forecasted_values_7$Point.Forecast)
actual_values_7_with_date <- cbind(date_dict_forecast,actual_values_7)

# Create date character
forecasted_values_7_with_date$date_char <- as.character(forecasted_values_7_with_date$date)
actual_values_7_with_date$date_char <- as.character(actual_values_7_with_date$date)

# Create the graphs with hover label
graphs_7 <- plot_ly(data = forecasted_values_7_with_date, x = forecasted_values_7_with_date$date, y = forecasted_values_7_with_date$`forecasted_values_7$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_7_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_7 <- graphs_7 %>% add_trace (data = actual_values_7_with_date, x = actual_values_7_with_date$date, y = actual_values_7_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_7_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_7 <- graphs_7 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_7

# ++++

# Category: FOODS, State: California, Code: FCA +++

sales_split$data[[8]]
actual_sales_split$data[[8]]

# To check stationery

adf.test(sales_split$data[[8]]$units)

# Differencing

plot(sales_split$data[[8]]$units)

diff_data_8 <- diff (sales_split$data[[8]]$units, differences = 1)  # 1st differencing
plot(diff_data_8)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_8)
pacf(diff_data_8)

# Fit the ARIMA Model

seasonal_fit_8_versionA <- arima(sales_split$data[[8]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_8_versionB <- arima(sales_split$data[[8]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_8_stepwise <-auto.arima(sales_split$data[[8]]$units, seasonal = TRUE)
seasonal_fit_8_search <-auto.arima(sales_split$data[[8]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_8_versionA, seasonal_fit_8_versionB)

# Validate the Model

checkresiduals(seasonal_fit_8_versionB)

# Forecast

forecasted_results_8 <- forecast(seasonal_fit_8_versionB, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_8 <- data.frame(forecasted_results_8)
actual_values_8 <- actual_sales_split$data[[8]]$units
accuracy(forecasted_values_8$Point.Forecast, actual_values_8)

# Visualize The Forecast

# Binding the date with values
forecasted_values_8_with_date <- cbind(date_dict_forecast,forecasted_values_8$Point.Forecast)
actual_values_8_with_date <- cbind(date_dict_forecast,actual_values_8)

# Create date character
forecasted_values_8_with_date$date_char <- as.character(forecasted_values_8_with_date$date)
actual_values_8_with_date$date_char <- as.character(actual_values_8_with_date$date)

# Create the graphs with hover label
graphs_8 <- plot_ly(data = forecasted_values_8_with_date, x = forecasted_values_8_with_date$date, y = forecasted_values_8_with_date$`forecasted_values_8$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_8_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_8 <- graphs_8 %>% add_trace (data = actual_values_8_with_date, x = actual_values_8_with_date$date, y = actual_values_8_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_8_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_8 <- graphs_8 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_8

# ++++

# Category: FOODS, State: California, Code: FCA +++

sales_split$data[[9]]
actual_sales_split$data[[9]]

# To check stationery

adf.test(sales_split$data[[9]]$units)

# Differencing

plot(sales_split$data[[9]]$units)

diff_data_9 <- diff (sales_split$data[[9]]$units, differences = 1)  # 1st differencing
plot(diff_data_9)

# Identify ARIMA parameters (p, d, q)

acf(diff_data_9)
pacf(diff_data_9)

# Fit the ARIMA Model

seasonal_fit_9_versionA <- arima(sales_split$data[[9]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(3,3,3), period=7))
seasonal_fit_9_versionB <- arima(sales_split$data[[9]]$units, 
                                 order=c(5,0,1), 
                                 seasonal=list(order=c(2,0,2), period=7))
seasonal_fit_9_stepwise <-auto.arima(sales_split$data[[9]]$units, seasonal = TRUE)
seasonal_fit_9_search <-auto.arima(sales_split$data[[9]]$units, stepwise = FALSE, seasonal = TRUE)

# Checking which model is the best

AICc(seasonal_fit_9_versionA, seasonal_fit_9_versionB)

# Validate the Model

checkresiduals(seasonal_fit_9_versionA)

# Forecast

forecasted_results_9 <- forecast(seasonal_fit_9_versionA, h=28)  # Forecast next 28 periods

# Evaluating The Forecast

forecasted_values_9 <- data.frame(forecasted_results_9)
actual_values_9 <- actual_sales_split$data[[9]]$units
accuracy(forecasted_values_9$Point.Forecast, actual_values_9)

# Visualize The Forecast

# Binding the date with values
forecasted_values_9_with_date <- cbind(date_dict_forecast,forecasted_values_9$Point.Forecast)
actual_values_9_with_date <- cbind(date_dict_forecast,actual_values_9)

# Create date character
forecasted_values_9_with_date$date_char <- as.character(forecasted_values_9_with_date$date)
actual_values_9_with_date$date_char <- as.character(actual_values_9_with_date$date)

# Create the graphs with hover label
graphs_9 <- plot_ly(data = forecasted_values_9_with_date, x = forecasted_values_9_with_date$date, y = forecasted_values_9_with_date$`forecasted_values_9$Point.Forecast`, type = 'scatter', mode = 'lines+markers',
                    text = forecasted_values_9_with_date$date_char, hoverinfo = 'text+y', name = 'Forecasted Sales')
graphs_9 <- graphs_9 %>% add_trace (data = actual_values_9_with_date, x = actual_values_9_with_date$date, y = actual_values_9_with_date$actual_values, type = 'scatter', mode = 'lines+markers',
                                    text = actual_values_9_with_date$date_char, hoverinfo = 'text+y', name = 'Actual Sales')

# Custumize the hover label
graphs_9 <- graphs_9 %>% layout(hoverlabel = list(bgcolor = "white"))

graphs_9

# ++++
summary(seasonal_fit_1_versionA)
seasonal_fit_1_versionA
seasonal_fit_2_versionA
seasonal_fit_3_versionA
seasonal_fit_4_versionA
seasonal_fit_5_versionB
seasonal_fit_6_versionB
seasonal_fit_7_versionA
seasonal_fit_8_versionB
seasonal_fit_9_versionA


view(cbind(forecasted_values_7$Point.Forecast, 
           forecasted_values_8$Point.Forecast, 
           forecasted_values_9$Point.Forecast))

view(cbind(actual_values_7, 
           actual_values_8,
           actual_values_9))

graphs_1
graphs_2
graphs_3
graphs_4
graphs_5
graphs_6
graphs_7
graphs_8
graphs_9

