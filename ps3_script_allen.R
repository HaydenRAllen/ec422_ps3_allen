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

#I added geom_smooth to the plot to show the trend more clearly. 
#The trend shows a general increase in gasoline sales over time 
  
#3
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

#The shape changed to a typical logarithmic curve. The seasonal swings
#are less pronounced in the log graph, but still noticeable.
#I don't think the log transformation is a good idea. The only noticeable difference
#is the shape of the trend.


retail_ts <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL, y = "Log of Millions of Dollars"
  ) +
  theme_minimal()

retail_ts |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Services Sales (Log Scale)",
    x = NULL, y = "Log of Millions of Dollars"
  ) +
  theme_minimal()




