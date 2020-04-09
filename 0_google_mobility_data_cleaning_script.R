# TODO:(yohance.nicholas) - Determine how to use purrr, jsonlite and mapp to import json data for all ccaricom countries with iso2c list

library(jsonlite)
AG <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/AG/mobility.json")
BS <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/BS/mobility.json")
BB <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/BB/mobility.json")
BZ <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/BZ/mobility.json")
DM <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/DM/mobility.json")
GD <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/GD/mobility.json")
GY <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/GY/mobility.json")
HT <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/HT/mobility.json")
JM <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/JM/mobility.json")
KN <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/KN/mobility.json")
LC <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/LC/mobility.json")
VC <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/VC/mobility.json")
SR <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/SR/mobility.json")
TT <- fromJSON("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/TT/mobility.json")



caricom_countries <- list(caricom_covid$iso2c)
caricom_countries
library(broom)


kable(caricom_covid$iso2c)
iso2c <- caricom_covid$iso2c
iso2c
