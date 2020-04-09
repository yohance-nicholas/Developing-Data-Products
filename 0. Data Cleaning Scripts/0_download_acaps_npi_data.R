# Download ACAPS non-pharmaceutical interventions data
# Adapted from tidycovid19 script prepared by Joachim Gassen
# Downloads non-pharmaceutical interventions (NPI) data related to Covid-19
# from the ACAPS governmental measures database
# ({https://www.acaps.org/covid19-government-measures-dataset}).

url <- "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"

selector_path <- paste0(
  "#data-resources-0 > div > ul > li > ",
  "div.hdx-btn-group.hdx-btn-group-fixed > ",
  "a.btn.btn-empty.btn-empty-blue.hdx-btn.resource-url-analytics.ga-download"
)

dta_url <- xml2::read_html(url) %>%
  rvest::html_node(css = selector_path) %>% rvest::html_attr('href')

tmp_file <- tempfile(".xlsx")
utils::download.file(paste0("https://data.humdata.org", dta_url), tmp_file,
                     quiet = TRUE, mode = "wb")
raw_dta <- readxl::read_excel(tmp_file, sheet = "Database")

acaps_npi_data <- raw_dta

acaps_npi_data <- raw_dta
names(acaps_npi_data) <-tolower(names(acaps_npi_data))
names(acaps_npi_data)[16] <- "alternative_source"

# Some spelling inconsistencies:
acaps_npi_data$category[acaps_npi_data$category == "Movement Restriction"] <- "Movement restrictions"
acaps_npi_data$category[acaps_npi_data$category == "Movement Restrictions"] <- "Movement restrictions"
acaps_npi_data$category[acaps_npi_data$category == "Social and Economic Measures"] <- "Social and economic measures"
acaps_npi_data$category[acaps_npi_data$category == "Social Distancing"] <- "Social distancing"

acaps_npi_data <- acaps_npi_data %>%
  dplyr::select(-.data$pcode) %>% 
  dplyr::filter(!is.na(.data$date_implemented),
                !is.na(.data$category)) %>%
  dplyr::rename(iso3c = .data$iso) %>%
  dplyr::mutate(timestamp = Sys.time())

write.csv(acaps_npi_data, sprintf("acaps_npi_data_%s.csv", Sys.Date()))