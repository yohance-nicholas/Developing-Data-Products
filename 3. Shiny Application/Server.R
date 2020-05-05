##################################################################################
# Developing Data Products
# Peer-graded Assignment: Course Project: Shiny Application and Reproducible Pitch
# server.R
# Prepared by Yohance Nicholas
# Date: May 5th 2020
# Github Repo: https://yohance-nicholas.github.io/Developing-Data-Products/
##################################################################################

# Load Required Data from Github Repo -------------------------------------
world_covid_regression_data <- readRDS(gzcon(url("https://raw.githubusercontent.com/yohance-nicholas/Developing-Data-Products/master/1.%20Tidy%20Data/world_covid_regression_data.rds")))
caricom_today <- readRDS(gzcon(url("https://raw.githubusercontent.com/yohance-nicholas/Developing-Data-Products/master/1.%20Tidy%20Data/caricom_today.Rds")))

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)

# SERVER
server <- function(input, output) {
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(world_covid_regression_data[,input$outcome] ~ world_covid_regression_data[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(world_covid_regression_data, options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plot(world_covid_regression_data[,input$indepvar], world_covid_regression_data[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(world_covid_regression_data[,input$outcome] ~ world_covid_regression_data[,input$indepvar]), col="red")
    lines(lowess(world_covid_regression_data[,input$indepvar],world_covid_regression_data[,input$outcome]), col="blue")
  }, height=400)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      addMarkers(lat = caricom_today$lat, 
                 lng = caricom_today$lng,
                 popupOptions = markerClusterOptions,
                 popup = paste("<b>", caricom_today$country, "</b> <br>",
                               "Date: ", caricom_today$date, "<br>",
                               "Confirmed Cases: ", caricom_today$confirmed, "<br>",
                               "Deaths:", caricom_today$deaths,"<br>",
                               "Recovered", caricom_today$recovered,"<br>",
                               "Income Group:", caricom_today$income, "<br>",
                               "Population Aged 65+:", caricom_today$pop_65_over_2018,"<br>",
                               "Diabetes prev. (% pop 20-79):", caricom_today$diabetes_20_79)) %>% 
      addCircleMarkers(lat = caricom_today$lat, 
                       lng = caricom_today$lng,
                       weight = 1,
                       radius = caricom_today$confirmed_per_100k,
                       color = 'yellow') %>% 
      addCircleMarkers(lat = caricom_today$lat, 
                       lng = caricom_today$lng,
                       weight = 1,
                       radius = caricom_today$deaths_per_100k,
                       color = 'red') %>%
      addLegend("bottomright", 
                colors= c('red', 'yellow'), 
                labels= c('Deaths per 100,000', 'Confirmed Cases per 100,000'), 
                title="Legend")})
    
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(world_covid_regression_data[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(world_covid_regression_data[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
}