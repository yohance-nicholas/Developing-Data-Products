# CARICOM Covid Data Cleaning Script
# Prepared by Yohance Nicholas
# April 2020

library(tidyverse)
library(tsibble)
library(tidyr)
tidycovid19 <- readRDS(gzcon(url("https://git.io/JfYa7")))

# Add Spatial Coordinates
library(wbstats)
series <- c("ST.INT.ARVL","SP.POP.0014.TO.ZS", "SP.POP.1564.TO.ZS", "SP.POP.65UP.TO.ZS", "SH.STA.DIAB.ZS", "SH.DTH.NCOM.ZS", "SH.DYN.NCOM.ZS")
wb_data <- wb(indicator = series,
              mrv = 1) %>% 
  select(iso3c, value, indicatorID) %>% 
  spread(indicatorID, value) %>% 
  rename(tourist_arrivals = ST.INT.ARVL,
         pop_0_14_2018 = SP.POP.0014.TO.ZS,
         pop_15_64_2018 = SP.POP.1564.TO.ZS,
         pop_65_over_2018 = SP.POP.65UP.TO.ZS,
         diabetes_20_79 = SH.STA.DIAB.ZS,
         death_by_ncd = SH.DTH.NCOM.ZS,
         death_by_cvd_ca_dm_30_70 = SH.DYN.NCOM.ZS)

wb_countries <- wbcountries() %>% 
  select(iso3c,
         lat,
         long) 

wb_data <- wb_data %>%  left_join(wb_countries,
                                  by = "iso3c")

tidycovid19 <- tidycovid19 %>%  left_join(wb_data,
                                          by = "iso3c")%>% 
  unite(popup, 
        c("country","confirmed"), 
        sep = ",", 
        remove = FALSE) %>% 
  select(-popup,popup)

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
         mortality_rate = deaths/confirmed*100,
         recovery_rate = recovered/confirmed*100,
         lat = ifelse(country == "Dominica", 15.41500, lat),
         long = ifelse(country == "Dominica", -61.3710, long),
         lat = ifelse(country == "Saint Kitts and Nevis", 17.35782, lat),
         long = ifelse(country == "Saint Kitts and Nevis", -62.7830, long),
         lat = as.numeric(lat),
         long = as.numeric(long)) %>% 
  mutate_at(35:37, round, 2) %>% 
  rename(lng = long)

caricom_today <- caricom_tidycovid19 %>% 
  filter(date == max(date))

caricom <- caricom_today %>% pull(iso3c)
by_economy_type <- group_by(caricom_today, economy)
by_income <- group_by(caricom_today, income)
by_oecs <- group_by(caricom_today, oecs)

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

top_6_deaths <- caricom_today %>%
  filter(date == max(date)) %>%
  group_by(`iso3c`) %>%
  summarise(value = sum(deaths, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(6) %>%
  select(`iso3c`) %>%
  pull()

top_6_deaths_per_100k <- caricom_today %>%
  filter(date == max(date)) %>%
  group_by(`iso3c`) %>%
  summarise(value = sum(deaths_per_100k, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(6) %>%
  select(`iso3c`) %>%
  pull()

top_6_mortality <- caricom_today %>%
  filter(date == max(date)) %>%
  group_by(`iso3c`) %>%
  summarise(value = sum(mortality_rate, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(6) %>%
  select(`iso3c`) %>%
  pull()

top_6_recovery <- caricom_today %>%
  filter(date == max(date)) %>%
  group_by(`iso3c`) %>%
  summarise(value = sum(recovery_rate, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(6) %>%
  select(`iso3c`) %>%
  pull()

# Clean Data for Multiple Regression Model
caricom_covid_regression_data <- data.frame(caricom_today %>% 
                                              select(country,
                                                     confirmed,
                                                     confirmed_per_100k,
                                                     deaths,
                                                     deaths_per_100k,
                                                     mortality_rate,
                                                     population,
                                                     pop_density,
                                                     pop_0_14_2018,
                                                     pop_15_64_2018,
                                                     pop_65_over_2018,
                                                     diabetes_20_79,
                                                     death_by_ncd,
                                                     death_by_cvd_ca_dm_30_70,
                                                     tourist_arrivals,
                                                     gdp_capita))

# Export Data Set ---------------------------------------------------------
# Time Series 
write.csv(caricom_tidycovid19, sprintf("caricom_tidycovid19.csv", Sys.Date()))
saveRDS(caricom_tidycovid19, sprintf("caricom_tidycovid19.rds", Sys.Date()))

# Cross Sectional
write.csv(caricom_today, sprintf("tidycovid19_caricom_today.csv", Sys.Date()))
saveRDS(caricom_today, sprintf("caricom_tidycovid19_today.rds", Sys.Date()))
write.csv(caricom_covid_regression_data, sprintf("caricom_covid_regression_data.csv", Sys.Date()))
saveRDS(caricom_covid_regression_data, sprintf("caricom_covid_regression_data.rds", Sys.Date()))

# Remove Unrequired Objects from the Environment --------------------------
rm("series", "wb_countries", "wb_data", "tidycovid19", "caricom_covid_regression_data")
