# CARICOM COVID-19 Cases Data Cleaning Script
# Cross-Sectional Dataset of Most Recent Data
# Prepared by Yohance Nicholas
# April 8th 2020

### Rationale

# The Coronavirus disease 2019 (COVID-19) is an infectious disease caused by
# severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). The disease was
# first identified in December 2019 in Wuhan, the capital of China's Hubei
# province, and has since spread globally, resulting in the ongoing 2019–20
# coronavirus pandemic. For this coursework project, I have opted to use Leaflet
# to map the incidence of the Novel Coronavirus among [CARICOM Member
# States](https://caricom.org/member-states-and-associate-members/). All CARICOM
# countries are classified as developing countries. They are all relatively
# small in terms of population and size, and diverse in terms of geography and
# population, culture and levels of economic and social development.  While the
# pandemic was slow to reach the CARICOM region, the begining of March saw the
# onset of the pandemic among CARICOM member states.

# Import Required Data ----------------------------------------------------
# Import most recent COVID-19 Confirmed Cases and Deaths
library(readr)
library(tidyverse)
library(tidyselect)
library(countrycode)

# Total Confirmed Cases
covid_19_confirmed <- tbl_df(read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols()))

covid_19_confirmed <- covid_19_confirmed %>% 
  select('Country/Region', Lat, Long, last_col()) %>% 
  rename(country = 'Country/Region',
         lat = Lat,
         lng = Long,
         confirmed = last_col()) %>%  
  mutate(iso3c = countrycode(country,
                             origin = "country.name",
                             destination = "iso3c"))

# Total Deaths
covid_19_deaths <- tbl_df(read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = cols()))

covid_19_deaths <- covid_19_deaths %>% 
  select('Country/Region', last_col()) %>% 
  rename(country = 'Country/Region',
         deaths = last_col()) %>%  
  mutate(iso3c = countrycode(country,
                             origin = "country.name",
                             destination = "iso3c")) %>% 
  select(-country)

covid_19_confirmed_and_deaths <- covid_19_confirmed %>% 
  left_join(covid_19_deaths, 
            by = 'iso3c') %>% 
  select(-iso3c,iso3c)

# Import Relevant Socio-Economic Data from the World Development Indicators Database

library(wbstats)
series <- c("NY.GDP.PCAP.KD", "SP.POP.TOTL", "SP.POP.0014.TO.ZS", "SP.POP.1564.TO.ZS", "SP.POP.65UP.TO.ZS", "AG.LND.TOTL.K2",
            "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN")

wb_data <- wb(indicator = series,
              mrv = 1) %>% 
  select(iso3c, value, indicatorID) %>% 
  spread(indicatorID, value) %>% 
  rename(gdp_capita_2018 = NY.GDP.PCAP.KD,
         pop_2018 = SP.POP.TOTL,
         pop_0_14_2018 = SP.POP.0014.TO.ZS,
         pop_15_64_2018 = SP.POP.1564.TO.ZS,
         pop_65_over_2018 = SP.POP.65UP.TO.ZS,
         land_area = AG.LND.TOTL.K2,
         pop_density = EN.POP.DNST,
         pop_largest_city = EN.URB.LCTY,
         life_expectancy = SP.DYN.LE00.IN)

wb_countries <- wbcountries() %>% 
  select(iso3c,
         iso2c,
         region,
         incomeID,
         income)

wb_data <- wb_data %>%  left_join(wb_countries,
            by = "iso3c")

### Merging the COVID-19 cases with the World Bank Data

# Now that both dataframes have been generated, I proceed to merge the two and
# select CARICOM member states for mapping. In order to facilitate the analysis
# of the severity of the pandemic country by country, three new variables will
# be generated:

# * Confirmed Cases per 100,000
# * Deaths per 100,000
# * Mortality Rate 

# Merge COVID-19 Confirmed Cases, World Bank Data and Filter CARICOM Countries
caricom_covid <- covid_19_confirmed_and_deaths %>% 
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
  arrange(country) %>% 
  left_join(wb_data, 
            by = 'iso3c') %>% 
  mutate(lat = if_else(country == "Belize", 17.1899, lat),
         lng = if_else(country == "Belize", -88.4976, lng),
         confirmed_per_100k = confirmed/pop_2018*100000,
         deaths_per_100k = deaths/pop_2018*100000,
         mortality_rate = deaths/confirmed*100)

by_income <- group_by(caricom_covid, income)
by_country <- group_by(caricom_covid, country)

# Export Clean Dataset
write.csv(caricom_covid, sprintf("caricom_covid_data_cs_%s.csv", Sys.Date()))
