# LOAD LIBRARIES ----
library(tidyverse)
library(data.table)

# LOAD DATA ----
covid_data_tbl <- fread("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# WRANGLE DATA ----
# __Rewrite country names to replace underscores ----
covid_data_tbl[, country := mapply(function(x) {stringr::str_replace_all(x, "_", " ")}, countriesAndTerritories)]

# __Select countries ----
countries <- c("Germany", "France", "United States of America", "Spain", "United Kingdom")

# __Select required columns ----
covid_data_tbl <- covid_data_tbl[!is.na(match(country, countries)),
    .(date=as.Date(dateRep, "%d/%m/%Y"), cases, country)]

# __Set key to order by date ----
setkey(covid_data_tbl, "date")

# __Add the cumulative column ----
covid_data_tbl[, cum_cases := cumsum(cases), by=country]

# __Get the label data ----
label_data <- covid_data_tbl[!is.na(match(country, countries[1:3])),
    .SD[.N, .(date, cum_cases)], by=country]

# VISUALIZE DATA ----
#Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")

ggplot() +
  geom_line(
    data = covid_data_tbl,
    mapping = aes(date, cum_cases, color=country),
    size = 1,
    key_glyph = "rect"
  ) +
  geom_label(
    data = label_data,
    aes(
      x = date,
      y = cum_cases,
      label = scales::number(cum_cases, accuracy = 1, big.mark = ".", decimal.mark = ","),
      color = country,
    ),
    hjust = 0,
    position = position_nudge(x=2),
    show.legend = FALSE
  ) +
  geom_blank(
    data = label_data,
    mapping = aes(x = date, y = cum_cases),
    position = position_nudge(x=32)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, accuracy = .1),
    breaks = seq(0, 20e+6, 25e+5)
  ) +
  scale_x_date(
    labels = scales::date_format("%B"),
    breaks = scales::date_breaks("1 month"),
    minor_breaks = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid = element_line(colour = "#F0F0F0"),
    legend.position = "bottom",
    legend.key=element_rect(fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_discrete(name = "Country") +
  labs(
    title = "COVID-19",
    subtitle = "2020 Cumulative Cases",
    x = "Date",
    y = "Cases (Mio)"
  )
