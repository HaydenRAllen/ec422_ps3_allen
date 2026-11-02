library(tidyverse)
library(fpp3)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)


retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent |>
  as_tibble() |>
  group_by(Month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)  

#2.6d
#Averaging smooths out the data and makes it harder to see trends.
#It does appear to be seasonal. You can see that the volume is higher 
#in the summer quarters and lower in the winter quarters.
 

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of Dollars", color = NULL
  ) +
  theme_minimal()

#2.7b
#At the end of every year, there is a drop. It seems to be a steep 
#rise in summer, then a drop through Fall and Winter. The SA series
#is smoother and has less variation than the NSA series. The trend 
#stays practically the same throughout the entire series.

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gasoline Sales",
    x = NULL, y = "Billions of Litres"
  ) +
  theme_minimal()

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gasoline Sales",
    x = NULL, y = "Billions of Litres"
  ) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal()

#2.7d
#I added geom_smooth to the plot to show the trend more clearly. 
#The trend shows a general increase in gasoline sales over time 
  
#3.1a
#The swings grow from 1960 to 1985, then they start to decrease.

can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Canadian Gasoline Sales (Log Scale)",
    x = NULL, y = "Log of Billions of Litres"
  ) +
  theme_minimal()

#3.1b
#The shape changed to a typical logarithmic curve. The seasonal swings
#are less pronounced in the log graph, but still noticeable.
#I don't think the log transformation is a good idea. The only noticeable difference
#is the shape of the trend.


retail_ts_filtered <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_ts_filtered |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL, y = "Millions of Dollars"
  ) +
  theme_minimal()

retail_ts_filtered |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Services Sales (Log Scale)",
    x = NULL, y = "Log of Millions of Dollars"
  ) +
  theme_minimal()

#3.1c
#The case for logging this series is stronger because it becomes more linear
#than when logging the other series. 

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gasoline Sales Growth (Direct)",
    x = NULL, y = "Percent"
  ) +
  theme_minimal()


can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Difference Between Growth Measures",
    x = NULL, y = "Percent"
  ) +
  theme_minimal()

#3.2
#the log difference approximation is more appropriate for this 
#series because it is less affected by the large swings in the data. 


retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom, aes(color = series_id)) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = "Series"
  ) +
  theme_minimal()


#3.3
#The adjusted series is much smoother and has less extreme swings.
#The not adjusted data is noisier because it has more variation
#and has the seasonal drop every year.

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

#4.1b
#The seasonally adjusted series has a much smoother line and trend compared
#to the NSA series. You can see the variation by the amount of dips in the NSA
#data.

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gasoline Sales (Log Scale)",
    y = "Log of Billions of Litres"
  ) +
  theme_minimal()

#4.1c
#I can see a seasonal decline starting in the Spring, continuing through
#the Summer, then a rise in the Fall, and then declining again in the winter.
#This makes sense because of things such as heating a house during the cold months


can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  ) 


retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

#4.3a
#The month with the largest seasonal factor seems to be December.
#It is roughly 0.2 over the trend and the size of the swings seem
#to decrease over time.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

#4.3b
#The appear to be fairly similar. The big difference is 2020 to 2023.


can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)


can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

#4.4a
#The estimated average monthly growth rate of gas production is 0.4%
#The month with the most significant seasonal effect is June


can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

#4.4b
#the tracks fit the actual series not well seeing as the actual 
#line is not linear


can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

#4.4c
#The residuals do not look like white noise.
#The p value tells us that we reject the null hypothesis of no autocorrelation


gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#5.1a
#The dates of the first and last forecast are January 1980 and March 2005.
#snaive forecasts more accurately and the metrics agree based on the low values


errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

#5.2a
#The DM statistic is -31.278 and the p-value is 2.2e-16
#The p value tell us there is a statistical significance in the difference
#between the models


gas_cv_fc_rw <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_rw |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#5.3a
#snaive MAE is 0.562 and RMSE is 0.702.
#tslm MAE is 2.09 and RMSE is 2.42
#the tslm model's accuracy changed more when switching methods.
#this is due to the rolling window only using more recent data
#rather than using all data, which would skew results.




