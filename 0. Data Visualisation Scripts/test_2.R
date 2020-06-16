
# test 1 ------------------------------------------------------------------
caricom_totals_ts %>% 
  select(-total_confirmed) %>% 
  gather(cases, value, total_deaths, total_recovered, total_active)

hc_area <- caricom_totals_ts %>% 
  select(-total_confirmed) %>% 
  gather(cases, value, total_deaths, total_recovered, total_active) %>% 
  hchart("area", 
         hcaes(x = date, y = value, group =cases)) %>% 
  hc_title(text = "Breakdown of Confirmed Cases Across CARICOM Member States") %>% 
  hc_plotOptions(area = list(
    stacking = "normal",
    lineColor = "#ffffff",
    lineWidth = 1,
    marker = list(
      lineWidth = 1,
      lineColor = "#ffffff"
    ))
  ) %>% hc_add_theme(hc_theme_ft())
hc_area

# test 1 ggplot2() --------------------------------------------------------

ggplot_area <- caricom_tidycovid19_cases %>% 
  filter(iso3c == "ATG") %>% 
  ggplot(aes(x = date, y = value, fill = cases)) +
  labs(title = "Breakdown of Confirmed Cases",
       x = "Date",
       y = "Total Cases") +
  geom_area()
ggplot_area
library(plotly)
ggplotly(ggplot_area)

# test 2 ------------------------------------------------------------------




caricom_tidycovid19_cases
hc_area_2 <- caricom_tidycovid19_cases %>% 
  filter(iso3c == "DMA") %>% 
  hchart("area",
         hcaes(x = date,
               y = value,
               group = cases))
hc_area_2



# test 3 ------------------------------------------------------------------


hc <- highchart() %>% 
  hc_chart(type = "area") %>% 
  hc_title(text = "Historic and Estimated Worldwide Population Distribution by Region") %>% 
  hc_subtitle(text = "Source: Wikipedia.org") %>% 
  hc_xAxis(categories = c("1750", "1800", "1850", "1900", "1950", "1999", "2050"),
           tickmarkPlacement = "on",
           title = list(enabled = FALSE)) %>% 
  hc_yAxis(title = list(text = "Percent")) %>% 
  hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             <b>{point.percentage:.1f}%</b> ({point.y:,.0f} millions)<br/>",
             shared = TRUE) %>% 
  hc_plotOptions(area = list(
    stacking = "percent",
    lineColor = "#ffffff",
    lineWidth = 1,
    marker = list(
      lineWidth = 1,
      lineColor = "#ffffff"
    ))
  ) %>% 
  hc_add_series(name = "Asia", data = c(502, 635, 809, 947, 1402, 3634, 5268)) %>% 
  hc_add_series(name = "Africa", data = c(106, 107, 111, 133, 221, 767, 1766)) %>%
  hc_add_series(name = "Europe", data = c(163, 203, 276, 408, 547, 729, 628)) %>% 
  hc_add_series(name = "America", data = c(18, 31, 54, 156, 339, 818, 1201)) %>% 
  hc_add_series(name = "Oceania", data = c(2, 2, 2, 6, 13, 30, 46)) 

hc