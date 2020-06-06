library(tidyverse)
library(countrycode)
covid_19_confirmed <- tbl_df(read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols()))
commodity <- c("GUY", "JAM", "TTO") # To create variable which identifies type of economy
oecs <- c("ATG", "DMA", "GRD", "KNA", "LCA", "VCT") # To create variable which identifies OECS Member States
caricom <- c("ATG","BHS","BLZ","BRB","DMA","GRD","GUY","HTI","JAM","KNA","LCA","SUR","TTO","VCT") # To create variable which identifies CARICOM Member States

caricom_confirmed <- covid_19_confirmed %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  rename(country = 'Country/Region') %>% 
  mutate(iso3c = countrycode(country,
                             origin = "country.name",
                             destination = "iso3c")) %>%
  filter(iso3c %in% caricom) %>% 
  add_row(country = "Caricom",
          iso3c = "CCM")

#TODO:(yohance.nicholas) Determine how to generate column totals using https://dplyr.tidyverse.org/reference/summarise_all.html
  write.csv(caricom_confirmed, file = "1. Tidy Data/caricom_confirmed.csv")
  
