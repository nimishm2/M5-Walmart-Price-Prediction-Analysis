# load datasets
d1 = "/Users/nimishmathur/Downloads/econ491 data/sell_prices.csv"
d2 = "/Users/nimishmathur/Downloads/econ491 data/calendar.csv"
d3 = "/Users/nimishmathur/Downloads/econ491 data/sales_train_validation.csv" #train
d4 = "/Users/nimishmathur/Downloads/econ491 data/sales_test_validation.csv" #test
d5 = "/Users/nimishmathur/Downloads/econ491 data/weights_validation.csv"

prices = read.csv(d1)
calendar = read.csv(d2)
sales1 = read.csv(d3)
sales2 = read.csv(d4)
w = read.csv(d5)

str(sales) #train

# load packages
library(tidyverse)
library(magrittr)
library(gridExtra)

# aggregate calendar
calendar %>% filter(!is.na(event_name_1)) %>%
  head(1) %>%
  select(date, weekday, event_name_1, event_type_1, event_name_2, event_type_2)

view(calendar)

# combine sales datasets
sales<-sales1 %>%
  left_join(sales2, by = c(
    "item_id", "dept_id", "cat_id", "store_id", "state_id"
  ))

rm(sales1, sales2)

# pivot
sales %<>% pivot_longer(cols = all_of(
  colnames(sales)[str_detect(colnames(sales), "d_")]
), names_to = "day_label", values_to = "units")

# aggregate to category/day
sales %<>% group_by(cat_id, day_label, state_id) %>% 
  summarise(units = sum(units)) %>% 
  ungroup()

head(sales)

date_dict = tibble("date" = seq.Date(from = as.Date("2011-01-29"),
                   to = as.Date("2016-06-19"), by = 1))

date_dict$day_label = paste0("d_", 1:nrow(date_dict))

#joining and sorting
sales %<>% left_join(date_dict, by = "day_label") %>% 
  select(-day_label) %>% 
  group_by(cat_id) %>%
  arrange(date, .by = T) %>% 
  ungroup()

#reorder the columns
sales %<>% select(cat_id, date, units, state_id)

head(sales)

# check dataset
str(sales)
summary(sales)
summary(calendar)
summary(prices)


# graphs
#view(prices)


# Create the line graph
ggplot(sales, aes(x = date, y = units, color = cat_id)) +
  geom_line(size = 0.25) +
  
  # Customize the appearance
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  
  # Set axis labels and title
  labs(x = "Date", y = "Units", title = "Walmart's Sales Data") +
  
  # Customize the legend
  scale_color_manual(values = c("red", "blue", "green")) +
  
  # Add a legend
  theme(legend.position = "top")

# create a histogram for each category
histogram_plots <- lapply(unique(sales$cat_id), function(cat) {
  data_subset <- sales[sales$cat_id == cat, ]
  ggplot(data_subset, aes(x = units)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "Units", y = "Frequency", title = paste("Histogram for cat_id =", cat))
})

# Print the histograms
print(histogram_plots[[1]])  # Histogram for cat_id 1 == FOODS
print(histogram_plots[[2]])  # Histogram for cat_id 2 == HOBBIES
print(histogram_plots[[3]])  # Histogram for cat_id 3 == HOUSEHOLDS


# Create separate boxplots for each cat_id
boxplot_plots <- lapply(unique(sales$cat_id), function(cat) {
  data_subset <- sales[sales$cat_id == cat, ]
  ggplot(data_subset, aes(x = factor(cat_id), y = units, fill = factor(cat_id))) +
    geom_boxplot(outlier.shape = 16) +  # Show outlier points as circles
    labs(x = "cat_id", y = "Units", title = paste("Boxplot for cat_id =", cat)) +
    theme(legend.position = "none")  # Remove legend for clarity
})

# Print the boxplots
print(boxplot_plots[[1]])  # Boxplot for cat_id == FOODS
print(boxplot_plots[[2]])  # Boxplot for cat_id == HOBBIES
print(boxplot_plots[[3]])  # Boxplot for cat_id == HOUSEHOLD

str(sales2)

# Create the line graph for 
ggplot(data = sales[which(sales$date>"2011-12-01" & sales$date<"2012-01-30"),], aes(x = date, y = units, color = cat_id)) +
  geom_line(size = 0.25) +
  
  # Customize the appearance
  scale_x_date(date_labels = "%d", date_breaks = "3 day") +
  
  # Set axis labels and title
  labs(x = "Date", y = "Units", title = "Walmart's Sales Data") +
  
  # Customize the legend
  scale_color_manual(values = c("red", "blue", "green")) +
  
  # Add a legend
  theme(legend.position = "top")

sales$time <- 1:nrow(sales)

# Create separate regressions for each cat_id
models <- lapply(unique(sales$cat_id), function(cat) {
  data_subset <- sales[sales$cat_id == cat, ]
  lm(units ~ time + I(time^2) + I(time^3), data=data_subset)
})

# Predict the next month's sales
# Assume 30 days in the next month
future_time <- (max(sales$time) + 1):(max(sales$time) + 30)

forecasted_sales <- lapply(1:length(models), function(i) {
  predict(models[[i]], newdata=data.frame(time=future_time))
})

# Display the forecasted sales
forecasted_sales

sales2 %<>% pivot_longer(cols = all_of(
  colnames(sales2)[str_detect(colnames(sales2), "d_")]
), names_to = "day_label", values_to = "units_test")

# Aggregate to category/day
sales2 %<>% group_by(cat_id, day_label, state_id) %>% 
  summarise(units_test = sum(units_test)) %>% 
  ungroup()

# Joining and sorting with date_dict
sales2 %<>% left_join(date_dict, by = "day_label") %>% 
  select(-day_label) %>% 
  group_by(cat_id) %>%
  arrange(date, .by = T) %>% 
  ungroup()

# Reorder the columns
sales2 %<>% select(cat_id, date, units_test, state_id)

# Convert forecasted sales from list to a tibble for plotting
forecasted_tibble <- tibble(
  date = rep(seq.Date(from = as.Date("2016-06-20"), by = "days", length.out = 30), times = 3),
  cat_id = rep(unique(sales$cat_id), each = 30),
  forecasted_units = unlist(forecasted_sales)
)

# Combine sales2 and forecasted_tibble for plotting
combined_data <- sales2 %>% left_join(forecasted_tibble, by = c("date", "cat_id"))

# Plot the results
ggplot(combined_data, aes(x = date)) +
  geom_line(aes(y = units_test, color = cat_id), size = 0.5) +
  geom_line(aes(y = forecasted_units, color = cat_id), linetype = "dashed", size = 0.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(x = "Date", y = "Units", title = "Walmart's Sales Data: Actual vs Forecasted") +
  theme(legend.position = "top")


print(forecasted_sales)
print(sum(is.na(combined_data$forecasted_units)))


ggplot(sales2, aes(x = date, y = units_test, color = cat_id)) +
  geom_line(size = 0.5) +
  labs(x = "Date", y = "Units", title = "Actual Sales Data") +
  theme(legend.position = "top")

ggplot(forecasted_tibble, aes(x = date, y = forecasted_units, color = cat_id)) +
  geom_line(size = 0.5, linetype = "dashed") +
  labs(x = "Date", y = "Units", title = "Forecasted Sales Data") +
  theme(legend.position = "top")
install.packages("forecast")
library(forecast)
# Fit ARIMA model for each cat_id
arima_models <- lapply(unique(sales$cat_id), function(cat) {
  data_subset <- ts(sales[sales$cat_id == cat, ]$units, frequency=365) # assuming daily data
  auto.arima(data_subset)
})

# Forecast the next month's sales
forecasts <- lapply(arima_models, function(model) {
  forecast(model, h=30)
})

# Extract forecasted values for plotting
forecasted_sales <- lapply(forecasts, function(f) f$mean)

# Convert forecasted sales from list to a tibble for plotting
forecasted_tibble <- tibble(
  date = rep(seq.Date(from = as.Date("2016-06-20"), by = "days", length.out = 30), times = 3),
  cat_id = rep(unique(sales$cat_id), each = 30),
  forecasted_units = unlist(forecasted_sales)
)






# Create separate ARIMA forecasts for each cat_id
arima_forecasts <- lapply(unique(sales$cat_id), function(cat) {
  data_subset <- ts(sales[sales$cat_id == cat, ]$units, start=c(2011,1,29), frequency=365)
  
  # Fit the ARIMA model
  fit <- auto.arima(data_subset)
  
  # Forecast for the next 30 days
  forecast(fit, h=30)$mean
})

# Combine forecast data for plotting as done in your previous code
forecasted_tibble_arima <- tibble(
  date = rep(seq.Date(from = as.Date("2016-06-20"), by = "days", length.out = 30), times = 3),
  cat_id = rep(unique(sales$cat_id), each = 30),
  forecasted_units = unlist(arima_forecasts)
)

combined_data_arima <- sales2 %>% left_join(forecasted_tibble_arima, by = c("date", "cat_id"))

# Plot the results
ggplot(combined_data_arima, aes(x = date)) +
  geom_line(aes(y = units_test, color = cat_id), size = 0.5) +
  geom_line(aes(y = forecasted_units, color = cat_id), linetype = "dashed", size = 0.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(x = "Date", y = "Units", title = "Walmart's Sales Data: Actual vs ARIMA Forecasted") +
  theme(legend.position = "top")



combined_data_arima <- combined_data_arima %>% 
  left_join(sales_test_validation, by = c("date", "cat_id"))

# Plot the results with the overlay of sales_test_validation
ggplot(combined_data_arima, aes(x = date)) +
  geom_line(aes(y = units_test, color = cat_id), size = 0.5) +
  geom_line(aes(y = forecasted_units, color = cat_id), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = units_validation, color = cat_id), size = 0.5, linetype = "dotdash") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(x = "Date", y = "Units", title = "Walmart's Sales Data: Actual vs ARIMA Forecasted vs Validation") +
  theme(legend.position = "top")
