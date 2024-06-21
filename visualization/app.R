# Import libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(forcats)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(DT)
# Import initial data

data <- read_csv("data/data.csv")
combined_data <- read_csv("data/data_for_model_evolution.csv")
author.activity <- read_csv("data/author_activity.csv")
author.activity |> dplyr::filter(str_detect(author_name, "bot$") == F) -> author.activity
color_palette <- "Paired"



ui <- fluidPage(
  titlePanel("Open Source AI - Deep Analysis"),
  tabsetPanel(
    tabPanel(
      "Model Type Popularity",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          dateRangeInput(
            "modelDateRange",
            "Select Date Range:",
            start = min(data$created_at, na.rm = T),
            end = max(data$created_at, na.rm = TRUE)
          ),
          checkboxGroupInput(
            "modelType",
            "Select Model Type:",
            choices = unique(data$TypeString),
            selected = unique(data$TypeString)
          ),
          selectInput(
            "aggregationLevel",
            "Select Aggregation Level:",
            choices = c(
              "Day" = "day",
              "Week" = "week",
              "Month" = "month"
            ),
            selected = "week"
          )
        ),
        mainPanel(
          plotOutput("plot1", height = "40vh", width = "70vw"),
          plotOutput("plot2", height = "40vh", width = "70vw")
        )
      )
    ),
    tabPanel("Author Activity", sidebarLayout(
      sidebarPanel(
        width = 3,
        dateRangeInput(
          "authorDateRange",
          "Select Date Range:",
          start = min(data$created_at, na.rm = T),
          end = max(data$created_at, na.rm = TRUE)
        ),
        uiOutput("authorSelect"),
        # Dynamic UI for selecting authors
      ),
      mainPanel(
        plotOutput("authorActivityPlot", height = "40vh", width = "70vw"),
        plotOutput(
          "authorActivityAveragePlot",
          height = "40vh",
          width = "70vw"
        )
      )
    )),
    tabPanel(
      "Total Models and Average Score",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          dateRangeInput(
            "plotDateRange",
            "Select Date Range:",
            start = min(data$created_at, na.rm = TRUE),
            end = max(data$created_at, na.rm = TRUE)
          ),
          checkboxGroupInput(
            "typeInput",
            "Select Type of Model:",
            choices = unique(data$TypeString),
            selected = unique(data$TypeString)
          ),
          selectInput(
            "aggregationLevel2",
            "Select Aggregation Level:",
            choices = c(
              "Day" = "day",
              "Week" = "week",
              "Month" = "month"
            ),
            selected = "day"
          )
        ),
        mainPanel(
          plotOutput("p_type_vs_average", height = "40vh", width = "70vw"),
          plotOutput("p_type_combined", height = "40vh", width = "70vw")
        )
      )
    ),
    tabPanel(
      "Models Architecture Popularity",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          dateRangeInput(
            "popularityDateRange",
            "Select Date Range:",
            start = min(data$created_at, na.rm = TRUE),
            end = max(data$created_at, na.rm = TRUE)
          ),
          checkboxGroupInput(
            "architecture",
            "Select Architectures of Interest",
            choices = unique(data$Architecture_new),
            selected = unique(data$Architecture_new)
          ),
          selectInput(
            "aggregationLevel",
            "Select Aggregation Level:",
            choices = c(
              "Day" = "day",
              "Week" = "week",
              "Month" = "month"
            ),
            selected = "week"
          )
        ),
        mainPanel(
          plotOutput("popularity1", height = "40vh", width = "70vw"),
          plotOutput("popularity2", height = "40vh", width = "70vw")
        )
      )
    ),
    tabPanel(
      "Evolution of Popular Base Models",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          checkboxGroupInput(
            "models",
            "Select Base Models of Interest:",
            choices = c(
              "Llama 1 30 b" = "llama30",
              "Llama 1 7b" = "llama7b7",
              "Llama 2 70b" = "llama270",
              "Llama 3 8b" = "llama38",
              "Llama 3 70b" = "llama370",
              "Mistral 7b" = "mistral7b7",
              "Mixtral 8x7 46b" = "mixtral8x7b46",
              "Phi 2" = "phi2",
              "Phi 3 mini" = "phi3mini",
              "Phi 3 medium" = "phi3medium"
            ),
            selected = unique(combined_data$Model)
          ),
          radioButtons(
            "labels",
            "Show Labels:",
            choices = list("Yes" = TRUE, "No" = FALSE),
            selected = TRUE
          ),
        ),
        mainPanel(plotOutput(
          "evolution",
          height = "90vh", width = "80vw"
        ))
      )
    ),
    tabPanel(
      "Effects of Important Models Release",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          dateRangeInput(
            "plotDateRange2",
            "Select Date Range:",
            start = as.Date("2023-01-01"),
            end = max(data$created_at, na.rm = TRUE)
          ),
          selectInput(
            "aggregation",
            "Select Aggregation Level:",
            choices = c(
              "Day" = "day",
              "Week" = "week",
              "Month" = "month"
            ),
            selected = "week"
          )
        ),
        mainPanel(
          plotOutput(
            "new_models_release_date",
            height = "70vh",
            width = "70vw"
          )
        )
      )
    )
  )
)
server <- function(input, output, session) {
  filteredModelData <- reactive({
    req(input$modelDateRange, input$modelType)
    data |>
      dplyr::filter(
        created_at >= input$modelDateRange[1],
        created_at <= input$modelDateRange[2],
        TypeString %in% input$modelType
      ) |>
      mutate(agg_date = floor_date(created_at, unit = input$aggregationLevel))
  })

  # Ensure the plots for model tracking are correctly defined (no changes needed here unless there are issues)
  output$plot1 <- renderPlot({
    df <- filteredModelData() |>
      group_by(TypeString, agg_date) |>
      reframe(new_models = n(), .groups = "drop") |>
      mutate(total_models = cumsum(new_models)) |>
      group_by(agg_date) |>
      reframe(
        TypeString,
        new_models,
        total_models,
        total_new_models_that_period = sum(new_models),
        .groups = "drop"
      )

    if (input$aggregationLevel == "day") {
      ggplot(df, aes(x = agg_date, y = new_models, fill = TypeString)) +
        geom_col() +
        labs(title = "New Models Over Time", x = "", y = "New Models") +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = c(0, 0)
        ) +
        scale_fill_brewer(palette = color_palette) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      ggplot(df, aes(x = agg_date, y = new_models, fill = TypeString)) +
        geom_col(color = "black", size = 0.15) +
        labs(title = "New Models Over", x = "", y = "New Models") +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = c(0, 0)
        ) +
        scale_fill_brewer(palette = color_palette) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })

  output$plot2 <- renderPlot({
    df <- filteredModelData() |>
      group_by(TypeString, agg_date) |>
      reframe(new_models = n(), .groups = "drop") |>
      group_by(agg_date) |>
      mutate(percentage = new_models / sum(new_models)) |>
      ungroup()
    if (input$aggregationLevel == "day") {
      ggplot(df, aes(x = agg_date, y = percentage, fill = TypeString)) +
        geom_col() +
        labs(title = "Percentage of New Models per Period", x = "Date", y = "Percentage") +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = c(0, 0)
        ) +
        scale_fill_brewer(palette = color_palette) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      ggplot(df, aes(x = agg_date, y = percentage, fill = TypeString)) +
        geom_col(color = "black", size = 0.15) +
        labs(title = "Percentage of New Models per Period", x = "Date", y = "Percentage") +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = c(0, 0)
        ) +
        scale_fill_brewer(palette = color_palette) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })

  # Authors Activity Plot logic
  topAuthors <- reactive({
    req(input$authorDateRange)
    author.activity |>
      inner_join(data, by = c("author_name" = "author")) |>
      dplyr::filter(
        created_at >= input$authorDateRange[1],
        created_at <= input$authorDateRange[2]
      ) |>
      group_by(author_name) |>
      reframe(
        total_repos = sum(num_of_repos),
        .groups = "drop"
      ) |>
      arrange(desc(total_repos)) |>
      slice_head(n = 10) |>
      pull(author_name)
  })

  output$authorSelect <- renderUI({
    # Use checkboxGroupInput for multi-selection
    checkboxGroupInput(
      "selectedAuthors",
      "Select Authors from Top 10:",
      choices = topAuthors(),
      selected = topAuthors()
    )
  })

  output$authorActivityPlot <- renderPlot({
    req(input$authorDateRange, input$selectedAuthors)
    df <- author.activity |>
      dplyr::filter(author_name %in% input$selectedAuthors) |>
      arrange(desc(num_of_repos)) |>
      head(10) |>
      inner_join(data, by = c("author_name" = "author")) |>
      dplyr::filter(
        created_at >= input$authorDateRange[1],
        created_at <= input$authorDateRange[2]
      ) |>
      group_by(author_name, TypeString, created_at) |>
      reframe(Average = mean(Average), .groups = "drop")

    ggplot(
      df,
      aes(
        x = created_at,
        y = author_name,
        fill = TypeString,
        color = author_name
      )
    ) +
      geom_line(
        linetype = "solid",
        linewidth = 1.3,
        show.legend = F
      ) +
      geom_tile(color = "black") +
      labs(
        title = "Author Activity",
        x = "",
        y = "All models by user",
        color = ""
      ) +
      theme_minimal() +
      scale_color_brewer(palette = color_palette) +
      scale_fill_brewer(palette = color_palette) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top",
        axis.text.y = element_blank(),
        plot.margin = margin(
          l = 15,
          r = 0,
          t = 0,
          b = 0
        )
      )
  })

  output$authorActivityAveragePlot <- renderPlot({
    req(input$authorDateRange, input$selectedAuthors)
    df <- author.activity |>
      dplyr::filter(author_name %in% input$selectedAuthors) |>
      arrange(desc(num_of_repos)) |>
      head(10) |>
      inner_join(data, by = c("author_name" = "author")) |>
      dplyr::filter(
        created_at >= input$authorDateRange[1],
        created_at <= input$authorDateRange[2]
      ) |>
      group_by(author_name, TypeString, created_at) |>
      reframe(Average = mean(Average), .groups = "drop")

    ggplot(df, aes(x = created_at, y = Average, color = author_name)) +
      geom_point(size = 1.5, alpha = 0.5) +
      geom_line() +
      labs(title = "Average Score of Models created by Activity", x = "Date", y = "Average Score") +
      theme_minimal() +
      scale_color_brewer(palette = color_palette) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top"
      )
  })

  output$p_type_vs_average <- renderPlot({
    data |>
      dplyr::filter(!is.na(TypeString)) |>
      dplyr::filter(
        created_at >= as.Date(input$plotDateRange[1]) &
          created_at <= as.Date(input$plotDateRange[2]),
        TypeString %in% input$typeInput
      ) |>
      mutate(agg_date = floor_date(created_at, input$aggregationLevel2)) |>
      ggplot(aes(x = agg_date, y = Average, color = TypeString)) +
      geom_point(alpha = 0.4, position = position_dodge2()) +
      geom_smooth(se = FALSE, linewidth = 2) +
      labs(title = "Type vs Average", x = "", y = "Average") +
      scale_color_brewer(palette = color_palette) +
      scale_x_date(
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = c(0, 0)
      ) +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })

  output$p_type_combined <- renderPlot({
    req(input$plotDateRange, input$typeInput) # Ensure these inputs are not NULL

    unique_dates <- sort(unique(data$created_at))

    # Select every 5th date
    selected_dates <- unique_dates[seq(1, length(unique_dates), by = 1)]

    # Filter data based on user input
    filtered_data <- data |>
      dplyr::filter(
        created_at >= as.Date(input$plotDateRange[1]) &
          created_at <= as.Date(input$plotDateRange[2]),
        TypeString %in% input$typeInput
      ) |>
      dplyr::filter(!is.na(TypeString)) |>
      mutate(agg_date = floor_date(created_at, input$aggregationLevel2)) |>
      group_by(agg_date, TypeString) |>
      reframe(total = n(), .groups = "drop") |>
      group_by(TypeString) |>
      arrange(agg_date, .by_group = TRUE) |>
      mutate(total_cumsum = cumsum(total)) |>
      ungroup() |>
      # Complete the data frame with missing dates
      complete(TypeString,
        agg_date = seq(input$plotDateRange[1], input$plotDateRange[2], by = "day")
      ) |>
      # Fill missing cumulative sums
      group_by(TypeString) |>
      fill(total_cumsum, .direction = "down") |>
      dplyr::filter(agg_date %in% selected_dates) |>
      ungroup()

    if (nrow(filtered_data) == 0) {
      print("No data available for selected filters.")
      return(NULL)
    }


    # Generate the plot
    ggplot(
      filtered_data,
      aes(x = agg_date, y = total_cumsum, color = TypeString)
    ) +
      geom_line(
        aes(group = TypeString),
        position = "dodge",
        linewidth = 2,
        alpha = 0.8
      ) +
      labs(title = "Total Models of Specific Type", x = "Date", y = "Total") +
      scale_color_brewer(palette = color_palette) +
      theme(
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid = element_blank()
      ) +
      scale_x_date(
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = c(0, 0)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

  output$popularity1 <- renderPlot({
    # Get unique architecture names and sort them
    unique_architectures <- data$Architecture_new |>
      unique() |>
      sort()

    # Calculate change in popularity of models over time compared to the previous week
    data |>
      select(Params, created_at, Architecture_new) |>
      dplyr::filter(
        created_at >= as.Date(input$popularityDateRange[1]) &
          created_at <= as.Date(input$popularityDateRange[2]),
        Architecture_new %in% input$architecture
      ) |>
      na.omit() |>
      arrange(created_at) |>
      mutate(agg_date = floor_date(created_at, input$aggregationLevel)) |>
      group_by(agg_date, Architecture_new) |>
      reframe(total_that_agg_date = n(), .groups = "drop") |>
      complete(
        agg_date = seq(min(agg_date), max(agg_date), by = input$aggregationLevel),
        Architecture_new,
        fill = list(total_that_agg_date = 0)
      ) |>
      group_by(Architecture_new) |>
      mutate(change = total_that_agg_date - dplyr::lag(total_that_agg_date, default = 0)) -> change_in_popularity

    # Plot change in popularity of models over time compared to the previous week
    change_in_popularity |>
      ggplot(aes(x = agg_date, y = change, fill = Architecture_new)) +
      stat_smooth(
        method = "loess",
        se = FALSE,
        geom = "area",
        alpha = 0.6
      ) +
      labs(title = "Change in Popularity of Models Over Time", x = "Date", y = "Change in Popularity") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_brewer(palette = color_palette) +
      scale_x_date(
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = c(0, 0)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

  output$popularity2 <- renderPlot({
    data |>
      select(Params, created_at, Architecture_new) |>
      dplyr::filter(
        created_at >= as.Date(input$popularityDateRange[1]) &
          created_at <= as.Date(input$popularityDateRange[2]),
        Architecture_new %in% input$architecture
      ) |>
      na.omit() |>
      arrange(created_at) |>
      mutate(agg_date = floor_date(created_at, input$aggregationLevel)) |>
      group_by(agg_date, Architecture_new) |>
      reframe(total_that_agg_date = n(), .groups = "drop") |>
      complete(
        agg_date = seq(min(agg_date), max(agg_date), by = input$aggregationLevel),
        Architecture_new,
        fill = list(total_that_agg_date = 0)
      ) |>
      group_by(Architecture_new) |>
      mutate(change = total_that_agg_date - dplyr::lag(total_that_agg_date, default = 0)) -> change_in_popularity


    change_in_popularity |>
      group_by(agg_date) |>
      reframe(
        average_that_agg_date = mean(total_that_agg_date),
        total_that_agg_date,
        Architecture_new
      ) |>
      mutate(change = total_that_agg_date - average_that_agg_date) |>
      ggplot(aes(x = agg_date, y = change, fill = Architecture_new)) +
      stat_smooth(
        method = "loess",
        se = FALSE,
        geom = "area",
        alpha = 0.6
      ) +
      labs(title = "Change in Popularity of Models Over Time Compared to Average Model Popularity", x = "Date", y = "Change in Popularity") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_brewer(palette = color_palette) +
      scale_x_date(
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = c(0, 0)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_smooth(
        aes(agg_date, average_that_agg_date),
        method = "loess",
        se = FALSE,
        color = "#464d49",
        linewidth = 0.5,
        show.legend = F
      )
  })

  output$evolution <- renderPlot({
    if (input$labels == TRUE) {
      combined_data |>
        dplyr::filter(Model %in% input$models) |>
        ggplot(aes(x = days_since_release, y = Average, color = Model)) +
        geom_line() +
        geom_point() +
        geom_label_repel(aes(label = original_name),
          max.overlaps = 50,
          size = 3
        ) +
        labs(title = "Evolution of Models Over Time", x = "Days since release", y = "Average Score Increase") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_continuous(breaks = seq(0, max(combined_data$days_since_release), by = 5)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_color_brewer(palette = color_palette)
    } else {
      combined_data |>
        dplyr::filter(Model %in% input$models) |>
        ggplot(aes(x = days_since_release, y = Average, color = Model)) +
        geom_line() +
        geom_point() +
        labs(title = "Model Comparison Over Time", x = "Days since release", y = "Average Score Increase") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_continuous(breaks = seq(0, max(combined_data$days_since_release), by = 5)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_color_brewer(palette = color_palette)
    }
  })

  output$new_models_release_date <- renderPlot({
    # Ensure Architecture_new exists in the data
    if (!"Architecture_new" %in% colnames(data)) {
      stop("Column 'Architecture_new' not found in data.")
    }

    # Create a list of release dates and labels
    release_dates <- data.frame(
      date = as.Date(
        c(
          "2024-01-20",
          "2024-02-08",
          "2023-07-18",
          "2023-02-24",
          "2023-12-06",
          "2023-09-20",
          "2024-05-22",
          "2024-04-10",
          "2023-10-17",
          "2024-01-11",
          "2024-04-22"
        )
      ),
      label = c(
        "Qwen2-beta",
        "Gemma",
        "LLama2",
        "LLama1",
        "Mistral7b-v0.2 and Mixtral 8x7B",
        "Mistral7b-v0.1",
        "Mistral7b-v0.3",
        "Mixtral 8x22B",
        "Phi1 and Phi1.5",
        "Phi2",
        "LLama3 and Phi3-mini"
      )
    )

    # Filter the release dates based on the input date range
    filtered_release_dates <- release_dates |>
      dplyr::filter(date >= as.Date(input$plotDateRange2[1]) &
        date <= as.Date(input$plotDateRange2[2]))

    # Create the plot data
    plot_data <- data |>
      select(Average, Params, created_at, Architecture_new) |>
      na.omit() |>
      arrange(created_at) |>
      dplyr::filter(
        created_at >= as.Date(input$plotDateRange2[1]) &
          created_at <= as.Date(input$plotDateRange2[2])
      ) |>
      mutate(agg_date = floor_date(created_at, input$aggregation)) |>
      group_by(agg_date, Architecture_new) |>
      summarise(
        Average = max(Average, na.rm = TRUE),
        total = n(),
        .groups = "drop"
      ) |>
      group_by(agg_date) |>
      mutate(total_agg_date = sum(total), .groups = "drop")

    # Calculate the maximum y-value
    max_y <- max(plot_data$total_agg_date, na.rm = TRUE) + 10

    if (input$aggregation == "day") {
      # Create the base plot
      p <- ggplot(
        plot_data,
        aes(x = agg_date, y = total, fill = Architecture_new)
      ) +
        geom_col() +
        labs(title = "Total models per date", x = "Date", y = "Total models per date") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = c(0, 0)
        ) +
        scale_fill_brewer(palette = color_palette) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

      # Add conditional model release dates
      for (i in seq_along(filtered_release_dates$date)) {
        p <- p +
          geom_vline(
            xintercept = filtered_release_dates$date[i],
            linetype = "dashed",
            color = "#5d6066"
          ) +
          annotate(
            "text",
            x = filtered_release_dates$date[i],
            y = max_y,
            label = filtered_release_dates$label[i],
            angle = 90,
            hjust = 0.8,
            vjust = -0.8,
            color = "#5d6066",
            size = 3
          )
      }

      print(p)
    } else {
      # Create the base plot
      p <- ggplot(
        plot_data,
        aes(x = agg_date, y = total, fill = Architecture_new)
      ) +
        geom_col(color = "black", size = 0.15) +
        labs(title = "Total new models per date", x = "Date", y = "Total models per date") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = c(0, 0)
        ) +
        scale_fill_brewer(palette = color_palette) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

      # Add conditional model release dates
      for (i in seq_along(filtered_release_dates$date)) {
        p <- p +
          geom_vline(
            xintercept = filtered_release_dates$date[i],
            linetype = "dashed",
            color = "#5d6066"
          ) +
          annotate(
            "text",
            x = filtered_release_dates$date[i],
            y = max_y,
            label = filtered_release_dates$label[i],
            angle = 90,
            hjust = 0.8,
            vjust = -0.8,
            color = "black",
            size = 3
          )
      }

      print(p)
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)
