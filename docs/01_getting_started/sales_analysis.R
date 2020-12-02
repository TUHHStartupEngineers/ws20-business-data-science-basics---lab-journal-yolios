# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
glimpse(orderlines_tbl)
glimpse(bikes_tbl)
glimpse(bikeshops_tbl)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by=c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by=c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(category, c("category.1", "category.2", "category.3"), " - ") %>%
  mutate(total.price = price*quantity) %>%
  select(-ends_with(".id"), -"...1") %>%
  bind_cols(select(bike_orderlines_joined_tbl, order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price, everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price) %>%
  transmute(year=lubridate::year(order_date), total_price) %>%
  group_by(year) %>%
  summarize(sales=sum(total_price), .groups="keep") %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"))

# Step 2 - Visualize
sales_by_year_plt <- ggplot(sales_by_year_tbl, aes(x=year, y=sales)) +
  geom_col(fill="#2DC6D6") +
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se=FALSE, formula = y~x) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")) +
  labs(
    title = "Revenue by year",
    subtitle = "Upward Trend",
    x = "",
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 1 ----

# Step 1 - Manipulate
sales_by_category_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, category=category_1, total_price) %>%
  transmute(year=lubridate::year(order_date), category, total_price) %>%
  group_by(year, category) %>%
  summarise(sales=sum(total_price), .groups="keep") %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"))

# Step 2 - Visualize
sales_by_category_plt <- ggplot(sales_by_category_tbl, aes(year, sales, fill=category)) +
  geom_col() +
  geom_smooth(method = "lm", se=FALSE, formula = y~x) +
  facet_wrap(~ category) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each category has an upward trend",
    fill = "Main category",
    x = "",
    y = "Revenue"
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
bike_orderlines_wrangled_tbl %>%
  writexl::write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>%
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>%
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
