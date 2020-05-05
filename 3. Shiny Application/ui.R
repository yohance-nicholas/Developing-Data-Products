##################################################################################
# Developing Data Products
# Peer-graded Assignment: Course Project: Shiny Application and Reproducible Pitch
# ui.R
# Prepared by Yohance Nicholas
# Date: May 5th 2020
# Github Repo: https://yohance-nicholas.github.io/Developing-Data-Products/
##################################################################################


# Launch Shiny User Interface ---------------------------------------------
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)

ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("Regression Model (Dataset: World COVID-19 Database as at May 5th 2020)"),
  sidebarLayout(
    sidebarPanel(
      h2("Overview"),
      p("This Shiny Application was created in partial fulfilment of the Developing Data Products Course which comprises one of the five courses necessary for the Data Science: Statistics and Machine Learning Specialization offered by Johns Hopikins University through Coursera."),
      p("This application allows for rudimentary exploratory data analysis of the spread of the COVID-19 Pandemic through a purpose built socioeconomic dataset using the most recent data. Furthermore, the app provides a map of the most recent estimates of the spread of the disease among CARICOM Member States"),
      h3("Prepared by"),
      "Yohance Nicholas | Date: May 5th 2020",
      tags$a(href = "https://www.linkedin.com/in/yohance-nicholas/", "LinkedIn"), "|",
      tags$a(href = "https://yohance-nicholas.github.io/Developing-Data-Products/", "Github"),
      br(),
      selectInput("outcome", label = h3("Dependent Variable"),
                  choices = list("Confirmed Cases per 100k" = "confirmed_per_100k",
                                 "Confirmed Deaths per 100k" = "deaths_per_100k",
                                 "Confirmed Cases" = "confirmed",
                                 "Deaths" = "deaths",
                                 "Recoveries" = "recovered",
                                 "Mortality Rate" = "mortality_rate"), selected = 1),
      
      selectInput("indepvar", label = h3("Explanatory variable"),
                  choices = list("GDP Per Capita" = "gdp_capita",
                                 "Population Over 65 Years of Age" = "pop_65_over_2018",
                                 "Population Under 15 Years of Age" = "pop_0_14_2018",
                                 "Population 20-79 with Diabetes" = "diabetes_20_79",
                                 "Mortality from NCDs" = "death_by_ncd",
                                 "Mortality from CVD, DM & Cancer" = "death_by_cvd_ca_dm_30_70",
                                 "Population Density" = "pop_density",
                                 "Tourist Arrivals" = "tourist_arrivals"), selected = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                  tabPanel("Distribution", # Plots of distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2")))
                  ),
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("CARICOM Map", leafletOutput("mymap")),
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))
