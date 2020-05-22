# CARICOM Today
library(tidyverse)
tidycovid19 <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/merged.RDS")))
  
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
           country == "Trinidad and Tobago") 

caricom_today <- caricom_tidycovid19 %>% 
  filter(date == max(date))

# Export Clean Dataset
write.csv(caricom_today, sprintf("tidycovid19_caricom_today_%s.csv", Sys.Date()))
saveRDS(caricom_tidycovid19_today, sprintf("caricom_tidycovid19_today_%s.rds", Sys.Date()))