library(data.table)

url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

covid_data_dt[countriesAndTerritories == "Germany" & 
    lubridate::month(dateRep, label = T, abbr = F) == "September"]


aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]


covid_data_dt[, deaths_per_capita := deaths / popData2019]


mt_dt <- data.table(mtcars)
mt_dt[, mileage_type := ifelse(mpg > 20, "high", "low")]

# Average cases and deaths in Germany in April
covid_data_dt[countriesAndTerritories == "Germany" & month == 4,
    .(m_cases = mean(cases),
    m_deaths = mean(deaths))]

# Number of days with over 1k deaths in the USA in May
covid_data_dt[countriesAndTerritories == "United_States_of_America" &
    month == 5 &
    deaths > 1000,
    .N]

# Number of days with over 1k deaths for each country
covid_data_dt[deaths > 1000, .N, by = countriesAndTerritories]


# Number of cars and mean mileage for each gear type
mt_dt[, .(m_mpg = mean(mpg), .N), by = gear]
