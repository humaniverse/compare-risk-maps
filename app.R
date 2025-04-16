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
library(networkD3)

# ---- Load data ----
# indices <- read_rds("data/indices.rds")
indices_ranks <- read_rds("data/indices-ranks.rds")
indices_quintiles <- read_rds("data/indices-quintile.rds")

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
        class = "flex-select index_1",
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
        class = "flex-select index_2",
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
  plotlyOutput("scatterplot", height = "100%"),
  
  # ---- Compare quintiles ----
  p(""),
  uiOutput("quintile_text"),
  sankeyNetworkOutput("quintile_comparison")
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
  
  selected_quintiles <- reactive({
    req(input$index_1, input$index_2)
    
    indices_quintiles %>%
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
            "The chart below compares <span class=index1>{tolower(names(indices_list)[indices_list == input$index_1])}</span> and 
           <span class=index2>{tolower(names(indices_list)[indices_list == input$index_2])}</span> for each Local Authority (dots).
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
    
    # Generate axes rank labels
    x_axis_rank_label <- ifelse(input$index_1 == "imd", "most deprived", "least resilient")
    y_axis_rank_label <- ifelse(input$index_2 == "imd", "most deprived", "least resilient")
    
    # Generate annotation labels
    x_annotation_lowest <- switch(
      input$index_1,
      "imd" = "More deprived",
      "sofi" = "Worse social fabric",
      "cri" = "Less resilient"
    )
    
    x_annotation_highest <- switch(
      input$index_1,
      "imd" = "Less deprived",
      "sofi" = "Stronger social fabric",
      "cri" = "More resilient"
    )
    
    y_annotation_lowest <- switch(
      input$index_2,
      "imd" = "More deprived",
      "sofi" = "Worse social fabric",
      "cri" = "Less resilient"
    )
    
    y_annotation_highest <- switch(
      input$index_2,
      "imd" = "Less deprived",
      "sofi" = "Stronger social fabric",
      "cri" = "More resilient"
    )
    
    p <- ggplot(plot_data, aes(x = index_1, y = index_2)) +
      # Add unit line
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
      
      geom_point(aes(text = str_glue("<b>{ltla23_name}</b>\n\n{names(indices_list)[indices_list == input$index_1]} rank: {index_1}\n{names(indices_list)[indices_list == input$index_2]} rank: {index_2}"))) +
      
      #ylim(0, 310) +
      labs(
        x = str_glue("{names(indices_list)[indices_list == input$index_1]} rank\n(1 = {x_axis_rank_label})"),
        y = str_glue("{names(indices_list)[indices_list == input$index_2]} rank\n(1 = {y_axis_rank_label})"),
        # title = paste("Scatterplot of", input$index_1, "vs", input$index_2)
      ) +
      theme_minimal()
    
    # Show regression line if checkbox is checked
    if (input$show_regression_line) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "blue")
    }
    
    ggplotly(p, tooltip = "text") |> 
      add_annotations(
        x = 20,
        y = 311,
        text = str_glue("◄ {x_annotation_lowest}"),
        bgcolor = "#008837",
        font = list(color = "white"),
        showarrow = F
      ) |>
      add_annotations(
        x = 280,
        y = 311,
        text = str_glue("{x_annotation_highest} ►"),
        bgcolor = "#008837",
        font = list(color = "white"),
        showarrow = F
      ) |> 
      add_annotations(
        x = 305,
        y = 60,
        text = str_glue("{y_annotation_lowest} ►"),
        textangle = 90,
        bgcolor = "#7b3294",
        font = list(color = "white"),
        showarrow = F
      ) |>
      add_annotations(
        x = 305,
        y = 240,
        text = str_glue("◄ {y_annotation_highest}"),
        textangle = 90,
        bgcolor = "#7b3294",
        font = list(color = "white"),
        showarrow = F
      ) |>
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          #"zoom",
          "pan",
          "select",
          #"zoomIn",
          #"zoomOut",
          "autoScale",
          #"resetScale",
          "lasso2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      ) |> 
      layout(
        # margin = list(t = 50),
        xaxis = list(
          range = list(-5, 310), 
          title = list(font = list(color = "#008837"))
        ),
        yaxis = list(
          range = list(-5, 325), 
          title = list(font = list(color = "#7b3294"))
        )
      )
  })
  
  # ---- Quintiles ----
  output$quintile_text <- renderUI({
    req(selected_quintiles())
    
    current_indices <- selected_quintiles()
    
    # How many LAs are in the same quintile in both indices?
    quintile_comparison <- 
      current_indices |> 
      mutate(same_quintile = index_1 == index_2) |>
      count(same_quintile) |> 
      mutate(prop = n / sum(n))
    
    n_same_quintile <- 
      quintile_comparison |> 
      filter(same_quintile) |> 
      pull(n)
    
    prop_same_quintile <- 
      quintile_comparison |> 
      filter(same_quintile) |> 
      pull(prop)
    
    # Find an example Local Authority that's in the worst 20% in one index but not in the other
    example_outlier <- 
      current_indices |> 
      filter(index_1 == 1 & index_2 != 1) |> 
      # Find one with the biggest discrepancy
      arrange(desc(index_2)) |>
      slice(1)
    
    example_outlier_name <- example_outlier$ltla23_name
    example_outlier_index_1 <- example_outlier$index_1
    example_outlier_index_2 <- example_outlier$index_2
    
    # Text output
    tags$div(
      tags$p(
        HTML(
          str_glue(
            "{scales::percent(prop_same_quintile, accuracy = 0.1)} of Local Authorities ({n_same_quintile} out of {nrow(current_indices)})
            are in the same quintile for both <span class=index1>{tolower(names(indices_list)[indices_list == input$index_1])}</span> 
            and <span class=index2>{tolower(names(indices_list)[indices_list == input$index_2])}</span>."
          )
        )
      ),
      tags$p(
        HTML(
          str_glue(
            "The chart below shows how Local Authorities in each quintile of the <span class=index1>{tolower(names(indices_list)[indices_list == input$index_1])}</span> map
            are spread across quintiles in the <span class=index2>{tolower(names(indices_list)[indices_list == input$index_2])}</span> map.
            Some Local Authorities might appear quite differently on each map; for example, <b>{example_outlier_name}</b>
            is in the worst 20% in terms of {tolower(names(indices_list)[indices_list == input$index_1])} but appears in 
            quintile {example_outlier_index_2} for {tolower(names(indices_list)[indices_list == input$index_2])}."
          )
        )
      )
    )
  })
  
  output$quintile_comparison <- renderSankeyNetwork({
    req(selected_quintiles())
    
    current_indices <- selected_quintiles() |> 
      na.omit()
    
    # Create sankey diagram showing the flow of LAs between quintiles
    links <- 
      current_indices |> 
      count(index_1, index_2, name = "value") |> 
      mutate(
        source = str_glue("{names(indices_list)[indices_list == input$index_1]} quintile {index_1}"),
        target = str_glue("{names(indices_list)[indices_list == input$index_2]} quintile {index_2}")
      ) |> 
      mutate(
        source = if_else(str_detect(source, "quintile 1"), paste0(source, " (worst 20%)"), source),
        target = if_else(str_detect(target, "quintile 1"), paste0(target, " (worst 20%)"), target)
      ) |> 
      select(source, target, value)
    
    nodes <- tibble(name = unique(c(links$source, links$target)))
    
    # With networkD3, connection must be provided using id, not using real name like in the links dataframe. So we need to reformat it.
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    # Make the Network
    sankeyNetwork(
      Links = links, Nodes = nodes,
      Source = "IDsource", Target = "IDtarget",
      Value = "value", NodeID = "name", 
      fontSize = 16,
      sinksRight = FALSE
    )
  })
}

# Run the app ----
shinyApp(ui, server)
