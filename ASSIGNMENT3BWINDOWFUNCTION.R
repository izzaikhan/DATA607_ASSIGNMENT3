# Assignment 3B: Window Functions (dplyr)
# Dataset: ggplot2::economics_long (multiple time-series items)

install.packages(c("ggplot2","dplyr","lubridate","zoo"))  # run once
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)

# 1) Load dataset (has date, variable/item, value)
df <- ggplot2::economics_long %>%
  select(date, item = variable, value) %>%   # rename variable -> item
  arrange(item, date) %>%
  mutate(year = year(date))

# 2) Calculate YTD average and 6-day moving average per item
# YTD avg = cumulative mean within each item-year
# 6-day MA = rolling mean over last 6 days within each item
results <- df %>%
  group_by(item, year) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    ytd_avg = cummean(value)
  ) %>%
  ungroup() %>%
  group_by(item) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    ma_6day = zoo::rollmean(value, k = 6, fill = NA, align = "right")
  ) %>%
  ungroup()

# 3) Show a sample (first 20 rows)
print(head(results, 20))

# 4) OPTIONAL: Save output for submission
write.csv(results, "Assignment3B_results.csv", row.names = FALSE)

# 5) OPTIONAL: show last 10 rows for each item (looks nice in screenshots)
tail_by_item <- results %>%
  group_by(item) %>%
  slice_tail(n = 10) %>%
  ungroup()

print(tail_by_item)
