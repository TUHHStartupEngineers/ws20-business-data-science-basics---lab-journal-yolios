library(tidyverse)

diamonds2 <- readRDS("00_data/diamonds2.rds")
diamonds2 %>% head(n=5)
diamonds2_long <- diamonds2 %>% pivot_longer(starts_with("20"), names_to="year", values_to="price")
diamonds2_long

diamonds3 <- readRDS("00_data/diamonds3.rds")
diamonds3 %>% head(n=5)
diamonds3_wide <- diamonds3 %>% pivot_wider(names_from="dimension", values_from="measurement")
diamonds3_wide

diamonds4 <- readRDS("00_data/diamonds4.rds")
diamonds4 %>% head(n=5)
diamonds4_separated <- diamonds4 %>% separate(dim, c("x", "y", "z"), "/", convert=T)
diamonds4_separated

diamonds5 <- readRDS("00_data/diamonds5.rds")
diamonds5 %>% head(n=5)
diamonds5_united <- diamonds5 %>% unite(clarity, clarity_prefix, clarity_suffix, sep="")
diamonds5_united
