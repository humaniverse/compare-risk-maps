# Load required packages
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(plotly)
library(sf)
library(geographr)

# ---- Load data ----
indices <- read_rds("data/indices.rds")

indices_list <- c(
  "Community Resilience" = "cri",
  "Social Fabric" = "sofi",
  "Deprivation" = "imd"
)

# ---- UI ----
ui <- page_fillable(
  includeCSS("styles.css"),
  
  tags$title("Compare vulnerability/resilience maps"),
  
  # The title includes an inline drop-down to choose a region.
  tags$p(
    tags$div(
      style = "align-items: center;",
      tags$span("Explore similarities and differences between "),
      
      # First index
      tags$div(
        class = "flex-select",
        style = "width: 227px",
        selectInput(
          "index_1",
          label = NULL,
          choices = indices_list,
          selectize = FALSE
        )
      ),
      
      tags$span(" and "),
      
      # Second index
      tags$div(
        class = "flex-select",
        style = "width: 227px",
        selectInput(
          "index_2",
          label = NULL,
          choices = indices_list,
          selectize = FALSE
        )
      )
    )
  ),
  
  theme = bs_theme("lumen", version = 5)
)

# Server ----
server <- function(input, output, session) {
}

# Run the app ----
shinyApp(ui, server)
