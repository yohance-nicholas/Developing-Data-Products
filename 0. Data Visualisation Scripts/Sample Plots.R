
source("0_tidycovid19_caricom_all.R", local = T)

# Create Function For Automatic Naming of Plots ---------------------------

# Source
# https://stackoverflow.com/questions/54752246/automatic-file-numbering-in-ggsave-as-in-png/54752618

fname = function(basename = 'CARICOM_COVID19_Plot_', fileext = 'png'){
  paste(basename, format(Sys.time(), " %b-%d-%Y %H-%M-%S."), fileext, sep="")
}

summarise(by_economy_type, sum(confirmed))
summarise(by_economy_type, sum(population))
summarise(by_economy_type, mean(confirmed_per_100k))
summarise(by_income, sum(confirmed))
summarise(by_income, sum(population))
summarise(by_income, mean(confirmed_per_100k))
summarise(by_income, mean(mortality_rate))

# Time Series Plots -------------------------------------------------------
# Confirmed Cases among CARICOM Member States
caricom_tidycovid19 %>%
  filter(date >= as.Date("2020-03-07")) %>%
  ggplot(aes(x = date, y = confirmed, col = country)) +
  geom_line() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Confirmed COVID-19 Cases among CARICOM Member States",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

# Confirmed Cases among Commodity Based Countries
caricom_tidycovid19 %>%
  filter(economy == "Commodity Based",
         date >= as.Date("2020-03-07")) %>%
  ggplot(aes(x = date, y = confirmed, col = country)) +
  geom_line() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Confirmed COVID-19 Cases among Commodity Based Economies",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

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
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

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
         caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

# Confirmed Cases among CARICOM Member States 
caricom_tidycovid19 %>% 
  filter(date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among CARICOM Member States",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

# Confirmed Cases among OECS Member States 
caricom_tidycovid19 %>% 
  filter(oecs == "OECS Member State",
         date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among OECS Member States",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

# Confirmed Cases among Service Based Economies
caricom_tidycovid19 %>% 
  filter(economy == "Service Based",
         date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among Service Based Economies",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

# Confirmed Cases among Commodity Based Economies
caricom_tidycovid19 %>% 
  filter(economy == "Commodity Based",
         date >= as.Date("2020-03-07")) %>% 
  ggplot(aes(x=date, y=confirmed, fill = country)) +
  geom_area() +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Cumulative COVID-19 Cases among Commodity Based Economies",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas")
ggsave(fname(), path = "2. Plots", dpi = 600)

# Log Ratios --------------------------------------------------------------

# Growth among most affected CARICOM Member States
caricom_tidycovid19 %>% mutate(cases_logratio = difference(log(confirmed))) %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-15")) %>%
  ggplot(aes(x = date, y = cases_logratio, col = country)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ country, ncol = 3) +
  labs(x = "Date",
      y = "Confirmed Cases",
      title = "Growth in Cumulative COVID-19 Cases among Top 6 Worst Affected CARICOM Member States",
      caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
ggsave(fname(), path = "2. Plots", dpi = 600)

# Growth among most affected OECS Member States
caricom_tidycovid19 %>% mutate(cases_logratio = difference(log(confirmed))) %>%
    filter(iso3c %in% oecs, 
           date >= as.Date("2020-03-15")) %>%
    ggplot(aes(x = date, y = cases_logratio, col = country)) +
    geom_point() +
    geom_smooth(method = "loess") +
    facet_wrap(. ~ country, ncol = 3) +
    labs(x = "Date",
         y = "Confirmed Cases",
         title = "Growth in Cumulative COVID-19 Cases among OECS Member States",
         caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas") +
    ggthemes::scale_color_colorblind()
ggsave(fname(), path = "2. Plots", dpi = 600)
  

# Daily Increase in Cumulative Cases --------------------------------------
# Countries to highlight

# Daily Increase in Worst Affected Countries
gg <- caricom_tidycovid19 %>%
  mutate(cases_logratio = difference(log(confirmed))) %>%
  filter(iso3c %in% top_6) %>%
  filter(date >= as.Date("2020-03-26")) %>%
  ggplot(aes(x = date, y = cases_logratio, col = country)) +
  geom_hline(yintercept = log(2)/c(2:7,14,21), col='grey') +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(
    "Daily increase in cumulative cases",
    breaks = log(1+seq(0,60,by=10)/100),
    labels = paste0(seq(0,60,by=10),"%"),
    minor_breaks=NULL,
    sec.axis = sec_axis(~ log(2)/(.),
                        breaks = c(2:7,14,21),
                        name = "Doubling time (days)")) +
  labs(x = "Date",
       y = "Confirmed Cases",
       title = "Daily Increase in Cumulative Cases among Top 6 Worst Affected CARICOM Member States",
       caption = "Source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
ggsave(fname(), path = "2. Plots", dpi = 600)
library(plotly)
ggplotly(gg)


# Movement ----------------------------------------------------------------
# Grocery

# Recreational
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
  ylab("Percentage Change Compared to Baseline") +
  ggtitle("Mobility Changes among 6 Worst Affected CARICOM Member States") +
  labs(caption = "Source: Google COVID-19 Community Mobility Reports
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
recreation_facet
ggsave(fname(), path = "2. Plots", dpi = 600)
library(plotly)
ggplotly(recreation_facet)


recreation <- caricom_tidycovid19 %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")) %>%
  ggplot(aes(x = date, y = gcmr_retail_recreation , col = country)) +
  geom_line(method = "loess")+
  xlab("Date") +
  ylab("Percentage Change Compared to Baseline") +
  ggtitle("Mobility Changes to Retail & Recreational Areas among 6 Worst Affected CARICOM Member States") +
  labs(caption = "Source: Google COVID-19 Community Mobility Reports
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
recreation
ggsave(fname(), path = "2. Plots", dpi = 600)
library(plotly)
ggplotly(recreation)


# Parks
parks_facet <-  caricom_tidycovid19 %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")
  ) %>%
  ggplot(aes(x = date, y = gcmr_parks , col = country)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ country, ncol = 3) +
  xlab("Date") +
  ylab("Percentage Change Compared to Baseline") +
  ggtitle("Mobility Changes around Public Parks among 6 Worst Affected CARICOM Member States") +
  labs(caption = "Source: Google COVID-19 Community Mobility Reports
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
parks_facet
ggsave(fname(), path = "2. Plots", dpi = 600)
library(plotly)
ggplotly(parks_facet)


parks <- caricom_tidycovid19 %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")) %>%
  ggplot(aes(x = date, y = gcmr_parks , col = country)) +
  geom_line(method = "loess")+
  xlab("Date") +
  ylab("Percentage Change Compared to Baseline") +
  ggtitle("Mobility Changes around Public Parks among 6 Worst Affected CARICOM Member States") +
  labs(caption = "Source: Google COVID-19 Community Mobility Reports
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
parks
ggsave(fname(), path = "2. Plots", dpi = 600)
library(plotly)
ggplotly(parks)


# Workplaces
workplaces_facet <-  caricom_tidycovid19 %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")
  ) %>%
  ggplot(aes(x = date, y = gcmr_workplaces , col = country)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ country, ncol = 3) +
  xlab("Date") +
  ylab("Percentage Change Compared to Baseline") +
  ggtitle("Mobility Changes around Workplaces among 6 Worst Affected CARICOM Member States") +
  labs(caption = "Source: Google COVID-19 Community Mobility Reports
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
workplaces_facet
ggsave(fname(), path = "2. Plots", dpi = 600)
library(plotly)
ggplotly(workplaces_facet)


workplaces <- caricom_tidycovid19 %>%
  filter(iso3c %in% top_6,
         date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")) %>%
  ggplot(aes(x = date, y = gcmr_workplaces , col = country)) +
  geom_line(method = "loess")+
  xlab("Date") +
  ylab("Percentage Change Compared to Baseline") +
  ggtitle("Mobility Changes around Workplaces among 6 Worst Affected CARICOM Member States") +
  labs(caption = "Source: Google COVID-19 Community Mobility Reports
       Prepared by: Yohance Nicholas") +
  ggthemes::scale_color_colorblind()
workplaces
ggsave(fname(), path = "2. Plots", dpi = 600)
library(plotly)
ggplotly(workplaces)



# Country Specific Mobility Plots -----------------------------------------

gcmr_data <- caricom_tidycovid19 %>% 
  select(country, iso3c, date, contains("gcmr")) %>% 
  filter(date >= as.Date("2020-03-01"),
         date <= as.Date("2020-04-05")) %>% 
  gather(gcmr, value, contains("gcmr")) %>% 
  drop_na()

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
