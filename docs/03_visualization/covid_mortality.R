# LOAD LIBRARIES ----
library(tidyverse)
library(data.table)
library(maps)

# LOAD DATA ----
covid_data_tbl <- fread("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# WRANGLE DATA ----
# __Rewrite country names ----
covid_data_tbl[, country := stringr::str_replace_all(countriesAndTerritories, "_", " ")]

covid_data_tbl <- covid_data_tbl %>%
  mutate(country = case_when(
    country == "United Kingdom" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Czechia" ~ "Czech Republic",
    TRUE ~ country
  ))

# __Select required columns ----
covid_data_tbl <- covid_data_tbl[,
    .(date=as.Date(dateRep, "%d/%m/%Y"), deaths, country, population=popData2019)]

# __Set key to order by date ----
setkey(covid_data_tbl, "date")

# __Add the cumulative death column ----
covid_data_tbl[, cum_deaths := cumsum(deaths), by=country]

# __Add mortality rate column ----
covid_data_tbl[, mortality_rate := cum_deaths / population]

# __Get most recent mortality rate ----
mortality_data <- covid_data_tbl[,
    .(mortality_rate=mortality_rate[.N]), by=country][order(-mortality_rate)]

# __Merge mortality rate to world ----
world <- map_data("world")

world_mortality <- merge(x=world, y=mortality_data, by.x="region", by.y="country", all.x=T, all.y=F) %>%
  arrange(group, order)

# VISUALIZE ----
ggplot() +
  geom_map(
    data = world_mortality,
    map = world_mortality,
    mapping =  aes(x=long, y=lat, map_id=region, fill=mortality_rate),
    show.legend = T
  ) +
  theme_void()+
  scale_fill_gradient(
    low = "#000000",
    high = "#FF0000",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill",
    labels = scales::label_number(scale = 1e+3, accuracy = .1, suffix = " \211"),
    breaks = seq(0, 200e-5, 50e-5)
  ) +
  labs(
    title = "Mortality Rate",
    subtitle = "COVID19",
    fill = "Mortality Rate",
    caption = format(Sys.time(), "%d.%m.%Y")
  )
