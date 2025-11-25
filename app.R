library(shiny)
library(fpp3)
library(stringr)
library(gt)
library(bslib)

# Data preparation
aus_wine_raw <- readr::read_csv("AustralianWines.csv", na = "*")
names(aus_wine_raw) <- c("Month", "Fortified", "Red", "Rose",
                        "Sparkling", "Sweet_white", "Dry_white")

aus_wine <- aus_wine_raw |>
  fill(Rose, .direction = "down") |>
  mutate(
    Month = mdy(str_replace(Month, "-", "-01-")),
    Month = yearmonth(Month)
  ) |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

# Fixed training cutoff
train_end <- yearmonth("1993 Dec")
forecast_end <- yearmonth("1994 Dec")

# UI
ui <- page_sidebar(
  title = "Australia-Wine-Analysis-Forecast-With-Shiny-Positron",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 300,
    selectInput(
      "varietal_viz",
      "Select Wine Varietal:",
      choices = unique(aus_wine$Varietal),
      selected = "Fortified"
    ),
    tags$div(
      style = "margin-bottom: 15px;",
      tags$label("Training Cutoff Date:"),
      tags$p("December 1993", style = "font-weight: bold; color: #e74c3c;")
    ),
    tags$div(
      style = "margin-bottom: 15px;",
      tags$label("Forecast End Date:"),
      tags$p("December 1994", style = "font-weight: bold; color: #8e44ad;")
    ),
    numericInput(
      "forecast_horizon",
      "Forecast Horizon (months):",
      value = 12,
      min = 1,
      max = 24,
      step = 1
    )
  ),
  
  navset_card_tab(
    nav_panel(
      "Visualization",
      plotOutput("plot_viz", height = "600px")
    ),
    nav_panel(
      "Model Building",
      layout_columns(
        col_widths = c(6, 6),
        gt_output("model_specs_table"),
        gt_output("training_accuracy_table")
      )
    ),
    nav_panel(
      "Forecast",
      gt_output("forecast_accuracy_table"),
      plotOutput("plot_forecast", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: training data for all varietals
  training_all <- reactive({
    aus_wine |>
      filter(Month <= train_end)
  })
  
  # Reactive: validation data for all varietals
  validation_all <- reactive({
    aus_wine |>
      filter(Month > train_end, Month <= forecast_end)
  })
  
  # Reactive: fit models for all varietals
  models_all <- reactive({
    training_all() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  # Reactive: future forecasts for all varietals (dynamic)
  fc_future_all <- reactive({
    req(models_all(), input$forecast_horizon)
    
    models_all() |>
      forecast(h = input$forecast_horizon)
  })
  
  # Reactive: validation forecasts for all varietals
  fc_val_all <- reactive({
    models_all() |>
      forecast(new_data = validation_all())
  })
  
  # Output: Visualization tab plot (single varietal)
  output$plot_viz <- renderPlot({
  wine_var <- aus_wine |>
    filter(Varietal == input$varietal_viz)
  
  fc_var <- fc_future_all() |>
    filter(Varietal == input$varietal_viz)
  
  # Convert yearmonth to Date for proper positioning
  train_date <- as.Date(train_end)
  forecast_date <- as.Date(forecast_end)
  
  autoplot(fc_var, wine_var) +
    geom_vline(xintercept = train_date, 
               linetype = "dashed", 
               color = "#e74c3c", 
               linewidth = 1) +
    geom_vline(xintercept = forecast_date, 
               linetype = "dashed", 
               color = "#8e44ad", 
               linewidth = 1) +
    labs(
      title = paste("Forecasts for", input$varietal_viz, "wine sales"),
      x = "Month",
      y = "Sales (thousands of liters)"
    ) +
    guides(colour = guide_legend(title = "Model")) +
    theme_minimal()
})
    # Output: Model specifications table
    output$model_specs_table <- render_gt({
  models_tbl <- models_all() |>
    as_tibble()

  #Turn model objects into readable strings
  model_specs <- models_tbl |>
    transmute(
      Varietal,
      TSLM  = as.character(TSLM),
      ETS   = as.character(ETS),
      ARIMA = as.character(ARIMA)
)

  # Build gt table
  model_specs |>
    gt() |>
    tab_header(title = "Model Specifications") |>
    cols_label(
      Varietal = "Wine Varietal",
      TSLM     = "TSLM Model",
      ETS      = "ETS Model",
      ARIMA    = "ARIMA Model"
    ) |>
    tab_style(
      style = cell_text(color = "#3498db", weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_text(color = "#2c3e50"),
      locations = cells_body()
    ) |>
    opt_row_striping()
})

  
  # Output: Training accuracy table
  output$training_accuracy_table <- render_gt({
    train_acc <- accuracy(models_all()) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
    
    train_acc |>
      gt() |>
      tab_header(title = "Training Accuracy (All Varietals)") |>
      cols_label(
        Varietal = "Wine Varietal",
        .model = "Model"
      ) |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2) |>
      tab_style(
        style = cell_text(color = "#3498db", weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(color = "#2c3e50"),
        locations = cells_body()
      ) |>
      opt_row_striping()
  })
  
  # Output: Forecast accuracy table
  output$forecast_accuracy_table <- render_gt({
    val_acc <- accuracy(fc_val_all(), validation_all()) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
    
    val_acc |>
      gt() |>
      tab_header(
        title = "Forecast Accuracy (All Varietals)",
        subtitle = "Validation period: 1994"
      ) |>
      cols_label(
        Varietal = "Wine Varietal",
        .model = "Model"
      ) |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2) |>
      tab_style(
        style = cell_text(color = "#3498db", weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(color = "#2c3e50"),
        locations = cells_body()
      ) |>
      opt_row_striping()
  })
  
  # Output: Forecast plot (all varietals)
  output$plot_forecast <- renderPlot({
    autoplot(fc_val_all(), aus_wine) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 2) +
      labs(
        title = "Validation Forecasts for All Wine Varietals (1994)",
        x = "Month",
        y = "Sales (thousands of liters)"
      ) +
      guides(colour = guide_legend(title = "Model")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)