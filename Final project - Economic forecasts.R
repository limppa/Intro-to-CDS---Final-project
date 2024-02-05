# LOADING LIBRARIES AND DATA


library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


# Read inflation and GDP data into a data frame
HICP <- read_excel("data/HICP euro area.xlsx")
GDP <- read_excel("data/GDP euro area.xlsx")


# LINE GRAPHS


# GDP graph

# Create a new data frame for the forecast 'legs'
forecast_data <- GDP %>%
  select(Year, real_gdp_growth, fc_03_months, fc_06_months, fc_12_months) %>%
  mutate(Prev_Year_GDP = lag(real_gdp_growth)) %>%
  filter(!is.na(fc_06_months))


# Reshape forecast_data from wide to long format
forecast_data_long <- forecast_data %>%
  select(Year, fc_03_months, fc_06_months, fc_12_months) %>%
  pivot_longer(cols = starts_with("fc_"), names_to = "Forecast_Period", values_to = "Value") %>%
  mutate(Type = "Forecast",
         Forecast_Period = case_when(
           Forecast_Period == "fc_03_months" ~ "3 month",
           Forecast_Period == "fc_06_months" ~ "6 month",
           Forecast_Period == "fc_12_months" ~ "12 month"
         )) %>%
  filter(!is.na(Value))  # Remove NA values

# Combine actual GDP data and forecast data
plot_data <- rbind(
  GDP %>% select(Year, Value = real_gdp_growth) %>% mutate(Type = "Actual"),
  forecast_data_long %>% select(Year, Value, Type)
)

# Limit the years to start from 1998 when first prediction data is available
plot_data <- plot_data %>% filter(Year >= 1998)

p <- ggplot() +
  # Add dotted line to demarcate zero
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 1) +
  # Plot actual GDP values with thick black line
  geom_line(data = plot_data %>% filter(Type == "Actual"), aes(x = Year, y = Value, group = Type, color = Type), size = 1.2) +
  # Plot forecasted values with thinner transparent red line
  geom_segment(data=forecast_data, aes(x=Year-1, y=Prev_Year_GDP, xend=Year, yend=fc_03_months), color = "#E69F00", size = 1) +
  geom_segment(data=forecast_data, aes(x=Year-1, y=Prev_Year_GDP, xend=Year, yend=fc_06_months), color = "#56B4E9", size = 1) +
  geom_segment(data=forecast_data, aes(x=Year-1, y=Prev_Year_GDP, xend=Year, yend=fc_12_months), color = "#009E73", size = 1) +
  # Add squares to actual GDP data points for improved visibility
  geom_point(data = plot_data %>% filter(Type == "Actual"), aes(x = Year, y = Value, color = Type), shape = 22, size = 3, fill = "black", show.legend = FALSE) +
  # Other plot components
  theme_minimal() +
  labs(title = "Euro area GDP growth with 3-, 6-, and 12-month forecasts", x = "Year", y = "GDP Growth, %") +
  scale_color_manual(values = c("Actual" = "black", "3 month" = "#E69F00", "6 month" = "#56B4E9", "12 month" = "#009E73")) +
  scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 2))

# Print the plot
print(p)



# Inflation graph

# Create a new data frame for the inflation forecast 'legs'
forecast2_data <- HICP %>%
  select(Year, Inflation, fc_03_months, fc_06_months, fc_12_months) %>%
  mutate(Prev_Year_Inflation = lag(Inflation)) %>%
  filter(!is.na(fc_06_months))

# Reshape from wide to long format
forecast2_data_long <- forecast2_data %>%
  select(Year, fc_03_months, fc_06_months, fc_12_months) %>%
  pivot_longer(cols = starts_with("fc_"), names_to = "Forecast_Period", values_to = "Value") %>%
  mutate(Type = "Forecast",
         Forecast_Period = case_when(
           Forecast_Period == "fc_03_months" ~ "3 month",
           Forecast_Period == "fc_06_months" ~ "6 month",
           Forecast_Period == "fc_12_months" ~ "12 month"
         )) %>%
  filter(!is.na(Value))  # Remove NA values

# Combine actual Inflation data and forecast data
plot2_data <- rbind(
  HICP %>% select(Year, Value = Inflation) %>% mutate(Type = "Actual"),
  forecast2_data_long %>% select(Year, Value, Type)
)

# Limit the years to start from 2013 when first prediction data is available
plot2_data <- plot2_data %>% filter(Year >= 2012)

p2 <- ggplot() +
  # Add dotted line to demarcate zero
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 1) +
  # Add dashed line to demarcate target inflation rate of 2%
  geom_hline(yintercept = 2, linetype = "dashed", color = "red", size = 1, alpha = 0.5) +
  # Plot actual Inflation values with thick black line
  geom_line(data = plot2_data %>% filter(Type == "Actual"), aes(x = Year, y = Value, group = Type, color = Type), size = 1.2) +
  # Plot forecasted values with thinner transparent lines in different colors
  geom_segment(data=forecast2_data, aes(x=Year-1, y=Prev_Year_Inflation, xend=Year, yend=fc_03_months), color = "#E69F00", size = 1) +
  geom_segment(data=forecast2_data, aes(x=Year-1, y=Prev_Year_Inflation, xend=Year, yend=fc_06_months), color = "#56B4E9", size = 1) +
  geom_segment(data=forecast2_data, aes(x=Year-1, y=Prev_Year_Inflation, xend=Year, yend=fc_12_months), color = "#009E73", size = 1) +
  # Add squares to actual Inflation data points for improved visibility
  geom_point(data = plot2_data %>% filter(Type == "Actual"), aes(x = Year, y = Value, color = Type), shape = 22, size = 3, fill = "black", show.legend = FALSE) +
  # Other plot components
  theme_minimal() +
  labs(title = "Euro area inflation rate with 3-, 6-, and 12-month forecasts", x = "Year", y = "Inflation Rate, %") +
  scale_color_manual(values = c("Actual" = "black", "3 month" = "#E69F00", "6 month" = "#56B4E9", "12 month" = "#009E73")) +
  scale_x_continuous(breaks = seq(min(plot2_data$Year), max(plot2_data$Year), by = 2))

# Print the plot
print(p2)

