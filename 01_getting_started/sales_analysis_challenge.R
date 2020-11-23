# Loading libraries ----
library(tidyverse)
library(readxl)

# Importing Files ----
orderlines_tbl <-
  read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

bikes_tbl <-
  read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")

bikeshops_tbl <-
  read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")


# Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by=c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by=c("customer.id" = "bikeshop.id"))


# Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(location, c("city", "state"), ", ") %>%
  mutate(total.price = price*quantity) %>%
  select(-ends_with(".id"), -"...1") %>%
  bind_cols(select(bike_orderlines_joined_tbl, order.id)) %>%
  select(order.id, contains("order"), contains("model"),
      contains("category"), price, quantity, total.price, everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))


# Analyzing Data ----
sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, location=state, total_price) %>%
  transmute(year=lubridate::year(order_date), location, total_price) %>%
  group_by(year, location) %>%
  summarise(sales=sum(total_price), .groups="keep") %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", decimal.mark = ",",
      prefix = "", suffix = " €"))


# Visualizing Data ----
ggplot(sales_by_location_tbl, aes(year, sales, fill=location)) +
  geom_col() +
  geom_smooth(method = "lm", se=FALSE, formula = y~x) +
  facet_wrap(~ location) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
      decimal.mark = ",", prefix = "", suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Revenue by year and location",
    subtitle = "Most locations have an upward trend",
    fill = "Location",
    x = "",
    y = "Revenue"
  )
