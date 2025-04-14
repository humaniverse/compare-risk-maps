# Load required packages
library(shiny)
library(shinyjs)
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
  "Deprivation" = "imd",
  "Social Fabric" = "sofi"
)

# ---- UI ----
ui <- page_fluid(
  includeCSS("styles.css"),
  
  useShinyjs(),
  
  # JavaScript to adjust width for a standard select input
  tags$head(
    tags$script(HTML("
      $(document).on('change', '#index_1', function(){
         // Get the text of the selected option
         var text = $('#index_1 option:selected').text();
         
         // Create a hidden span to measure the width of the text
         var $tempSpan = $('<span>').text(text).css({
             'font-family': 'Arial',
             'font-size': '16px',
             'white-space': 'nowrap',
             'visibility': 'hidden'
         }).appendTo('body');
         var textWidth = $tempSpan.width();
         $tempSpan.remove();
         
         // Set the width of the select element with extra padding (e.g., 30px)
         $(this).css('width', (textWidth + 30) + 'px');
      });
      
      $(document).on('change', '#index_2', function(){
         // Get the text of the selected option
         var text = $('#index_2 option:selected').text();
         
         // Create a hidden span to measure the width of the text
         var $tempSpan = $('<span>').text(text).css({
             'font-family': 'Arial',
             'font-size': '16px',
             'white-space': 'nowrap',
             'visibility': 'hidden'
         }).appendTo('body');
         var textWidth = $tempSpan.width();
         $tempSpan.remove();
         
         // Set the width of the select element with extra padding (e.g., 30px)
         $(this).css('width', (textWidth + 30) + 'px');
      });
      
      // Optionally, trigger the change event on document ready
      $(document).ready(function(){
         $('#index_1').trigger('change');
         $('#index_2').trigger('change');
      });
    "))
  ),
  
  title = "Compare vulnerability/resilience maps",
  
  # The title includes an inline drop-down to choose a region.
  tags$p(
    tags$div(
      style = "align-items: center;",
      tags$span("Explore similarities and differences between "),
      
      # First index
      tags$div(
        class = "flex-select",
        # style = "width: 227px",
        selectInput(
          "index_1",
          label = NULL,
          choices = indices_list,
          selected = "imd",
          selectize = FALSE
        )
      ),
      
      tags$span(" and "),
      
      # Second index
      tags$div(
        class = "flex-select",
        # style = "width: 227px",
        selectInput(
          "index_2",
          label = NULL,
          choices = indices_list,
          selected = "cri",
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
