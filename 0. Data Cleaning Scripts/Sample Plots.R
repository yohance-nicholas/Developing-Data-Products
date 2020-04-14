
source("0_tidycovid19_caricom_all.R", local = T)

# Time Series Plots -------------------------------------------------------
# Confirmed Cases among CARICOM Member States
caricom_tidycovid19 %>%
  filter(date >= as.Date("2020-03-07")) %>%
  ggplot(aes(x = date, y = confirmed, col = country)) +
  geom_line() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Confirmed COVID-19 Cases among CARICOM Member States",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE")

# Confirmed Cases among Commodity Based Countries
caricom_tidycovid19 %>%
  filter(economy == "Commodity Based",
         date >= as.Date("2020-03-07")) %>%
  ggplot(aes(x = date, y = confirmed, col = country)) +
  geom_line() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Confirmed COVID-19 Cases among Commodity Based Economies",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE")

# Confirmed Cases among Service Based Countries  
caricom_tidycovid19 %>%
    filter(economy == "Service Based",
           date >= as.Date("2020-03-07")
    ) %>%
    ggplot(aes(x = date, y = confirmed, col = country)) +
    geom_line() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Confirmed COVID-19 Cases among Service Based Economies",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE")
  
# Confirmed Cases among OECS Member States 
caricom_tidycovid19 %>%
    filter(oecs == "OECS Member State",
           date >= as.Date("2020-03-07")
    ) %>%
    ggplot(aes(x = date, y = confirmed, col = country)) +
    geom_line() +
    labs(x = "Date",
         y = "Confirmed Cases",
         title = "Confirmed COVID-19 Cases among OECS Member States",
         caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE") 

# Confirmed Cases among CARICOM Member States 
caricom_tidycovid19 %>% 
  filter(date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among CARICOM Member States",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE") 

# Confirmed Cases among OECS Member States 
caricom_tidycovid19 %>% 
  filter(oecs == "OECS Member State",
         date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among CARICOM Member States",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE") 

# Confirmed Cases among Service Based Economies
caricom_tidycovid19 %>% 
  filter(economy == "Service Based",
         date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among Service Based Economies",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE") 

# Confirmed Cases among Commodity Based Economies
caricom_tidycovid19 %>% 
  filter(economy == "Commodity Based",
         date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among Commodity Based Economies",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE") 

# Log Ratios --------------------------------------------------------------

# Growth among most affected CARICOM Member States
tidycovid19 %>% mutate(cases_logratio = difference(log(confirmed))) %>%
  filter(
    iso3c %in% top_6,
    date >= as.Date("2020-03-15")
  ) %>%
  ggplot(aes(x = date, y = cases_logratio, col = country)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ country, ncol = 3) +
  labs(x = "Date",
         y = "Confirmed Cases") +
  ggthemes::scale_color_colorblind()

# Growth among most affected OECS Member States
  tidycovid19 %>% mutate(cases_logratio = difference(log(confirmed))) %>%
    filter(
      iso3c %in% oecs,
      date >= as.Date("2020-03-15")
    ) %>%
    ggplot(aes(x = date, y = cases_logratio, col = country)) +
    geom_point() +
    geom_smooth(method = "loess") +
    facet_wrap(. ~ country, ncol = 3) +
    labs(x = "Date",
         y = "Confirmed Cases") +
    ggthemes::scale_color_colorblind()
  

# Daily Increase in Cumulative Cases --------------------------------------
# Countries to highlight

# Daily Increase in Worst Affected Countries
tidycovid19 %>%
  mutate(
    cases_logratio = difference(log(confirmed))
  ) %>%
  filter(iso3c %in% top_6) %>%
  filter(date >= as.Date("2020-03-26")) %>%
  ggplot(aes(x = date, y = cases_logratio, col = country)) +
  geom_hline(yintercept = log(2)/c(2:7,14,21), col='grey') +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Date") +
    scale_y_continuous(
    "Daily increase in cumulative cases",
    breaks = log(1+seq(0,60,by=10)/100),
    labels = paste0(seq(0,60,by=10),"%"),
    minor_breaks=NULL,
    sec.axis = sec_axis(~ log(2)/(.),
                        breaks = c(2:7,14,21),
                        name = "Doubling time (days)")
  ) +
  ggtitle("Daily Increase in Cumulative Cases among worst affected CARICOM Member States") +
  ggthemes::scale_color_colorblind()



# Movement ----------------------------------------------------------------
# Grocery

# Recreational
# Facet Wrap of countries with most cases
recreation_facet <-  caricom_tidycovid19 %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")
  ) %>%
  ggplot(aes(x = date, y = gcmr_retail_recreation , col = country)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ country, ncol = 3) +
  xlab("Date") +
  ggthemes::scale_color_colorblind()

recreation <- caricom_tidycovid19 %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")
  ) %>%
  ggplot(aes(x = date, y = gcmr_retail_recreation , col = country)) +
  geom_line(method = "loess") +
  xlab("Date") +
  ggthemes::scale_color_colorblind()

library(plotly)
ggplotly(recreation)
ggplotly(recreation_facet)

# Parks

# Workplaces



# Cross Sectional Plots ---------------------------------------------------

# Pie Chart ---------------------------------------------------------------
library(plotly)

confirmed_pie <- plot_ly(caricom_today, labels = ~country, values = ~confirmed, type = 'pie')
confirmed_pie <- fig %>% layout(title = 'Confirmed Cases among CARICOM Member States',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
confirmed_pie

deaths_pie <-  plot_ly(caricom_today, labels = ~country, values = ~deaths, type = 'pie')
deaths_pie <- fig %>% layout(title = 'Confirmed Cases among CARICOM Member States',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
deaths_pie



# Slider ------------------------------------------------------------------
# Slider for commodity Based Economies 
slider_df <- caricom_tidycovid19 %>%
  filter(date >= "2020-03-10") %>% 
  select(date, iso3c, confirmed) %>% 
  spread(iso3c, confirmed) 
  
caricom_service_slider <- plot_ly(slider_df, x = ~date)
caricom_service_slider <- caricom_service_slider %>% add_lines(y = ~TTO, name = "Trinidad and Tobago")
caricom_service_slider <- caricom_service_slider %>% add_lines(y = ~JAM, name = "Jamaica")
caricom_service_slider <- caricom_service_slider %>% add_lines(y = ~GUY, name = "Guyana")
caricom_service_slider <- caricom_service_slider %>% layout(
  title = "Confirmed Cases",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 7,
          label = "1 wk",
          step = "day",
          stepmode = "backward"),
        list(
          count = 14,
          label = "2 wk",
          step = "day",
          stepmode = "backward"),
        list(
          count = 21,
          label = "3 wk",
          step = "day",
          stepmode = "backward"),
        list(step = "all"))),
    
    rangeslider = list(type = "date")),
  
  yaxis = list(title = "Confirmed Cases"))

caricom_service_slider
