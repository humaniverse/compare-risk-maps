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
indices_ranks <- read_rds("data/indices-ranks.rds")

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
  
  theme = bs_theme("lumen", version = 5),
  
  title = "Compare vulnerability/resilience maps",
  
  # ---- Title with drop-down boxes ----
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
  
  # ---- Scatterplot ----
  uiOutput("scatterplot_text"),
  checkboxInput(
    "show_regression_line",
    label = "Show statistical relationship between the two maps?",
    value = FALSE,
    width = "100%"
  ),
  plotlyOutput("scatterplot", height = "100%")
)

# Server ----
server <- function(input, output, session) {
  # ---- Reactive data ----
  selected_ranks <- reactive({
    req(input$index_1, input$index_2)
    
    indices_ranks %>%
      select(
        ltla23_code,
        ltla23_name,
        !!sym(input$index_1),
        !!sym(input$index_2)
      ) %>%
      rename(
        index_1 = !!sym(input$index_1),
        index_2 = !!sym(input$index_2)
      )
  })
  
  # ---- Scatterplot ----
  output$scatterplot_text <- renderUI({
    req(selected_ranks())
    
    plot_data <- selected_ranks()
    
    # Find the Local Authority with the biggest difference in ranks on the two selected indices
    biggest_outlier <- 
      plot_data |> 
      mutate(rank_diff = abs(index_1 - index_2)) |>
      filter(rank_diff == max(rank_diff, na.rm = TRUE))
    
    biggest_outlier_name <- biggest_outlier$ltla23_name[1]
    biggest_outlier_index_1 <- biggest_outlier$index_1[1]
    biggest_outlier_index_2 <- biggest_outlier$index_2[1]
    
    # Create a text output with the number of points
    tags$div(
      tags$p(
        HTML(
          str_glue(
            "The chart below compares <b>{tolower(names(indices_list)[indices_list == input$index_1])}</b> and 
           <b>{tolower(names(indices_list)[indices_list == input$index_2])}</b> for each Local Authority (dots).
           Lower ranks represent the least resilient and most vulnerable/deprived areas."
          )
        )
      ),
      tags$p(
        HTML(
          str_glue(
            "Local Authorities that are closer to the dashed line are similarly vulnerable/resilient/deprived.
           The further away from the line, the more different they are. For example, <b>{biggest_outlier_name}</b>
           has a {tolower(names(indices_list)[indices_list == input$index_1])} rank of {biggest_outlier_index_1}
           but a {tolower(names(indices_list)[indices_list == input$index_2])} rank of {biggest_outlier_index_2}."
          )
        )
      )
    )
  })
  
  output$scatterplot <- renderPlotly({
    req(selected_ranks())
    
    plot_data <- selected_ranks()
    
    p <- ggplot(plot_data, aes(x = index_1, y = index_2)) +
      # Add unit line
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
      
      geom_point(aes(text = str_glue("<b>{ltla23_name}</b>\n\n{names(indices_list)[indices_list == input$index_1]} rank: {index_1}\n{names(indices_list)[indices_list == input$index_2]} rank: {index_2}"))) +
      
      labs(
        x = str_glue("{names(indices_list)[indices_list == input$index_1]} rank"),
        y = str_glue("{names(indices_list)[indices_list == input$index_2]} rank"),
        # title = paste("Scatterplot of", input$index_1, "vs", input$index_2)
      ) +
      theme_minimal()
    
    # Show regression line if checkbox is checked
    if (input$show_regression_line) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "blue")
    }
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the app ----
shinyApp(ui, server)
