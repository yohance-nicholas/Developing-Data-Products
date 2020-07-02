# Refresh Developing Data Products Repo
# Refresh Data
source("https://git.io/JfMbR", local = T)

# Render Presentations
library(rmarkdown)
rmarkdown::render("1_R_Markdown_and_Leaflet.Rmd")
rmarkdown::render("2_R_Markdown_Presentation.Rmd")
rmarkdown::render("3_Shiny_Application_and_Reproducible_Pitch.Rmd")