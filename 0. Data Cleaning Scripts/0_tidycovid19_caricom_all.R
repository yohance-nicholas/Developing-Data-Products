# CARICOM Covid Data Cleaning Script
# Prepared by Yohance Nicholas
# Version: May 28th 2020

library(tidyverse)
library(tsibble)
library(tidyr)
library(sjmisc)
library(zoo) # for rollmean()


# Create ISO3C Variables --------------------------------------------------
commodity <- c("GUY", "JAM", "TTO") # To create variable which identifies type of economy
oecs <- c("ATG", "DMA", "GRD", "KNA", "LCA", "VCT") # To create variable which identifies OECS Member States
caricom <- c("ATG","BHS","BLZ","BRB","DMA","GRD","GUY","HTI","JAM","KNA","LCA","SUR","TTO","VCT") # To create variable which identifies CARICOM Member States


# Import COVID-19 Data ----------------------------------------------------
tidycovid19 <- readRDS(gzcon(url("https://git.io/JfYa7")))%>% 
  drop_na(confirmed)

tidycovid19_cases <- tidycovid19 %>% 
  select(iso3c,
         country,
         date,
         confirmed,
         deaths,
         recovered) %>% 
  mutate(active = confirmed - deaths - recovered) %>% 
  select(-confirmed) %>% 
  gather(cases, value, deaths, recovered, active)
  
# Add Spatial Coordinates
library(wbstats)
series <- c("ST.INT.ARVL","SP.POP.0014.TO.ZS", "SP.POP.1564.TO.ZS", "SP.POP.65UP.TO.ZS", "SH.STA.DIAB.ZS", "SH.DTH.NCOM.ZS", "SH.DYN.NCOM.ZS")
wb_data <- wb_data(indicator = series,
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

wb_countries <- wb_countries() %>% 
  select(iso3c,
         lat,
         long) 

wb_data <- wb_data %>%  left_join(wb_countries,
                                  by = "iso3c")

tidycovid19 <- tidycovid19 %>%  left_join(wb_data,
                                          by = "iso3c")%>% 
  mutate(active = confirmed - deaths - recovered,
         confirmed_logratio = difference(log(confirmed)),
         confirmed_per_100k = confirmed/population*100000,
         deaths_per_100k = deaths/population*100000,
         mortality_rate = deaths/confirmed*100,
         recovery_rate = recovered/confirmed*100,
         new_cases = confirmed - lag(confirmed),
         ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right")) 

# Create Dummy Variables
tidycovid19 <- tidycovid19 %>% 
  select(region, income) %>% 
  to_dummy(region, income) %>% 
  bind_cols(tidycovid19) %>% 
  select(-contains("region"), contains("region")) %>% 
  select(-contains("income"), contains("income"))

# Create Filtered Dataset
caricom_tidycovid19 <- tidycovid19 %>% 
  filter(iso3c %in% caricom) %>% 
  mutate(economy = if_else(iso3c %in% commodity, "Commodity Based", "Service Based" ),
         oecs = if_else(iso3c %in% oecs, "OECS Member State", "Non-OECS Member State" ),
         caricom = if_else(iso3c %in% caricom, "CARICOM Member State", "Non-CARICOM Member State" ),
         lat = ifelse(iso3c == "DMA", 15.41500, lat),
         long = ifelse(iso3c == "DMA", -61.3710, long),
         lat = ifelse(iso3c == "KNA", 17.35782, lat),
         long = ifelse(iso3c == "KNA", -62.7830, long),
         lat = as.numeric(lat),
         long = as.numeric(long)) %>%
  rename(lng = long)%>%
  filter(date >= as.Date("2020-03-07")) 

caricom_tidycovid19_cases <- tidycovid19_cases %>% 
  filter(iso3c %in% caricom)%>%
  filter(date >= as.Date("2020-03-07")) 

caricom_today <- caricom_tidycovid19 %>% 
  filter(date == max(date))

world_today <- tidycovid19 %>% 
  filter(date == max(date))

caricom <- caricom_today %>% pull(iso3c)
by_economy_type <- group_by(caricom_today, economy)
by_income <- group_by(caricom_today, income)
by_oecs <- group_by(caricom_today, oecs)
by_caricom <- group_by(caricom_tidycovid19, caricom)


# Create Objects for Totals -----------------------------------------------

caricom_totals <- caricom_today %>%
  filter(date == max(date))  %>%
  summarise(total_confirmed = sum(confirmed, na.rm = T),
            total_deaths = sum(deaths, na.rm = T),
            total_recovered = sum(recovered, na.rm = T),
            total_active = sum(active, na.rm = T))

caricom_totals_ts <- caricom_tidycovid19 %>% 
  group_by(date, caricom) %>% 
  summarise(total_confirmed = sum(confirmed),
            total_active = sum(active),
            total_deaths = sum(deaths),
            total_recovered = sum(recovered)) 

# List Top N Countries ----------------------------------------------------


caricom_totals_ts
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
                                                     recovered,
                                                     active,
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
                                                     gdp_capita,
                                                     income,
                                                     oecs,
                                                     economy,
                                                     contains("region"),
                                                     contains("income"))) %>% 
  mutate(income = case_when(income == "Low income"  ~ 1, income == "Upper middle income"  ~ 2, income == "High income"  ~ 3),
         oecs = if_else(oecs == "OECS Member State", 1, 2),
         economy = if_else(economy == "Commodity Based", 1, 2)) %>% 
  column_to_rownames(var = "country")

# Clean Data for Multiple Regression Model
world_covid_regression_data <- data.frame(world_today %>% 
                                            select(country,
                                                   confirmed,
                                                   confirmed_per_100k,
                                                   deaths,
                                                   recovered,
                                                   active,
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
                                                   gdp_capita,
                                                   region,
                                                   income,
                                                   contains("region"),
                                                   contains("income"))) %>% 
  mutate(deaths = if_else(deaths == 0, 0.01, deaths), # To prevent 0 values from becoming infinity when transformed
         deaths_per_100k = if_else(deaths_per_100k == 0, 0.01, deaths_per_100k), # To prevent 0 values from becoming infinity when transformed
         log_confirmed = log(confirmed),
         log_confirmed_per_100k = log(confirmed_per_100k),
         log_deaths = log(deaths),
         log_deaths_per_100k = log(deaths_per_100k),
         log_recovered = log(recovered),
         log_active = log(active),
         income = case_when(income == "Low income"  ~ 1, 
                            income == "Lower middle income"  ~ 2, 
                            income == "Upper middle income"  ~ 3, 
                            income == "High income"  ~ 4),
         region = case_when(region == "East Asia & Pacific"  ~ 1, 
                            region == "Europe & Central Asia"  ~ 2, 
                            region == "Middle East & North Africa"  ~ 4, 
                            region == "North America	" ~ 5, 
                            region == "South Asia"  ~ 6, 
                            region == "Sub-Saharan Africa "  ~ 7,
                            region == "Latin America & Caribbean" ~ 3)) %>% 
  column_to_rownames(var = "country") %>% 
  na.omit()

# Export Data Set ---------------------------------------------------------
# Time Series 
write.csv(caricom_tidycovid19, "caricom_tidycovid19.csv")
saveRDS(caricom_tidycovid19, "caricom_tidycovid19.Rds")
saveRDS(tidycovid19, "world_tidycovid19.Rds")

# Cross Sectional
write.csv(caricom_today, "caricom_today.csv")
saveRDS(caricom_today, "caricom_today.Rds")
saveRDS(world_today, "world_today.Rds")

# Regression Data
write.csv(caricom_covid_regression_data, "caricom_covid_regression_data.csv")
saveRDS(caricom_covid_regression_data, "caricom_covid_regression_data.rds")
saveRDS(world_covid_regression_data, "world_covid_regression_data.rds")

# Remove Unrequired Objects from the Environment --------------------------
rm("series", "wb_countries", "wb_data", "tidycovid19", "tidycovid19_cases")
