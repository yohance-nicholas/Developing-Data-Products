# CARICOM Today
library(tidyverse)
library(tsibble)
tidycovid19 <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/merged.RDS")))

# Add Spatial Coordinates
library(wbstats)
wb_countries <- wbcountries() %>% 
  select(iso3c,
         lat,
         long)

tidycovid19 <- tidycovid19 %>%  left_join(wb_countries,
                                  by = "iso3c")

# Create Aggregate Variables
commodity <- c("GUY", "JAM", "TTO") # To create variable which identifies type of economy
oecs <- c("ATG", "DMA", "GRD", "KNA", "LCA", "VCT") # To create variable which identifies OECS Member States

# Create Filtered Dataset
caricom_tidycovid19 <- tidycovid19 %>% 
  filter(country == "Antigua and Barbuda"|
           country == "Bahamas"|
           country == "Barbados"|
           country == "Belize" |
           country == "Dominica"|
           country == "Grenada"|
           country == "Guyana"|
           country == "Haiti"|
           country == "Jamaica"|
           country == "Montserrat"|
           country == "Saint Kitts and Nevis"|
           country == "Saint Lucia"|
           country == "Saint Vincent and the Grenadines"|
           country == "Suriname"|
           country == "Trinidad and Tobago") %>% 
  mutate(economy = if_else(iso3c %in% commodity, "Commodity Based", "Service Based" ),
         oecs = if_else(iso3c %in% oecs, "OECS Member State", "Non-OECS Member State" ),
         confirmed_logratio = difference(log(confirmed)),
         confirmed_per_100k = confirmed/population*100000,
         deaths_per_100k = deaths/population*100000,
         mortality_rate = deaths/confirmed*100)

caricom_today <- caricom_tidycovid19 %>% 
  filter(date == max(date))

caricom <- caricom_today %>% pull(iso3c)
by_economy_type <- group_by(caricom_tidycovid19, economy)
by_income <- group_by(caricom_tidycovid19, income)
by_oecs <- group_by(caricom_tidycovid19, oecs)

# List Top N Countries ----------------------------------------------------
top_5 <- caricom_today %>%
  filter(date == max(date)) %>%
  group_by(`iso3c`) %>%
  summarise(value = sum(confirmed, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(5) %>%
  select(`iso3c`) %>%
  pull()

top_6 <- caricom_today %>%
  filter(date == max(date)) %>%
  group_by(`iso3c`) %>%
  summarise(value = sum(confirmed, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(6) %>%
  select(`iso3c`) %>%
  pull()

top_10 <- caricom_today %>%
  filter(date == max(date)) %>%
  group_by(`iso3c`) %>%
  summarise(value = sum(confirmed, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(10) %>%
  select(`iso3c`) %>%
  pull()

# Export Data Set ---------------------------------------------------------
# Time Series 
write.csv(caricom_tidycovid19, sprintf("caricom_tidycovid19_%s.csv", Sys.Date()))
saveRDS(caricom_tidycovid19, sprintf("caricom_tidycovid19_%s.rds", Sys.Date()))

# Cross Sectional
write.csv(caricom_today, sprintf("tidycovid19_caricom_today_%s.csv", Sys.Date()))
saveRDS(caricom_today, sprintf("caricom_tidycovid19_today_%s.rds", Sys.Date()))
