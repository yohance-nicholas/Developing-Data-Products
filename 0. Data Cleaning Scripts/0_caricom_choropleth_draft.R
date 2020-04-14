
# Clean Environment and Load Tidy CARICOM COVID-19 Data -------------------

# Attempted to adapt code from https://plotly.com/r/choropleth-maps/
rm(list = ls())
source("0_tidycovid19_caricom_all.R", local = T)

caricom_choropleth_df <- caricom_today %>% 
  select(iso3c, country, confirmed, deaths, recovered, income, economy, lat, long) %>% 
  mutate(hover = paste("Country", country, '<br>',
                       "Confirmed Cases", confirmed, '<br>',
                       "Deaths", deaths, '<br>',
                       "Recovered", recovered, '<br>',
                       "Income", income, '<br>',
                       "Economy", economy, '<br>'))

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

caricom_choropleth <- caricom_choropleth_df %>% 
  plot_geo() %>% 
  add_trace(z = ~confirmed,
            colors = ~confirmed,
            colors = 'Blues',
            text = ~hover,
            locations = ~iso3c,
            marker = list(line = l)) %>% 
  colorbar(title = 'Confirmed Cases',
           tickprefix = '$') %>% 
  layout(title = 'Choropleth Map of Confirmed Cases in CARICOM Member States',
         geo = g)

caricom_choropleth

caricom_choropleth

library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(df, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~total.exports, text = ~hover, locations = ~code,
  color = ~total.exports, colors = 'Purples'
)
fig <- fig %>% colorbar(title = "Millions USD")
fig <- fig %>% layout(
  title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
  geo = g
)

fig

map_data()