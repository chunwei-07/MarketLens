library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(tidyquant)
library(htmltools)
library(forecast)
library(DT)

# --- UI DEFINITION ---
ui <- page_navbar(
    title = "MarketLens",
    theme = bs_theme(
        version = 5,
        bootswatch = "darkly",
        base_font = font_google("Manrope"),
        primary = "#39a263",
        bg = "#121A16",
        fg = "#FFFFFF",
        "input-bg" = "#1A2420",
        "card-bg" = "#1A2420",
        "input-color" = "#FFFFFF"
    ),

    header = tags$head(
        tags$style(HTML("
            .navbar-brand, .navbar-nav .nav-link, .input-group-text {
                color: white !important;
            }
        "))
    ),

    # --- TABS ---
    # dashboard
    nav_panel(
        title = "Dashboard",
        layout_sidebar(
            sidebar = sidebar(
                title = "Stock Selection",
                width = 300,
                textInput(
                    inputId = "ticker",
                    label = "Enter Stock Ticker",
                    value = "AAPL"
                ),
                dateRangeInput(
                    inputId = "date_range",
                    label = "Select Date Range",
                    start = today() - years(1),
                    end = today()
                ),
                actionButton("analyze", "Analyze", class = "btn-primary w-100")
            ),
            # main content area for plot
            card(
                card_header("Stock Performance"),
                card_body(padding = "10px", plotlyOutput("stock_plot"))
            )
        )
    ),

    # forecasting
    nav_panel(
        title = "Forecasting",
        layout_sidebar(
            sidebar = sidebar(
                title = "Forecasting Settings", width = 300,
                radioButtons(
                    inputId = "model_choice",
                    label = "Model Selection",
                    choices = c("ARIMA" = "arima", "Prophet" = "prophet"),
                    selected = "arima"
                ),
                sliderInput(
                    inputId = "forecast_period",
                    label = "Forecast Period (Days)",
                    min = 7, max = 90, value = 30
                ),
                actionButton("generate_forecast", "Generate Forecast", class = "btn-primary w-100")
            ),
            # main content for forecast plot and metrics
            card(
                card_header("Forecast Plot"),
                card_body(padding = "10px", plotlyOutput("forecast_plot"))
            ),
            card(
                card_header("Error Metrics"),
                card_body(padding = "10px", DTOutput("error_metrics_table"))
            )
        )
    ),

    nav_panel("Analysis"),
    nav_panel("Reports"),
    nav_spacer(),
    nav_menu(
        title = "Settings",
        align = "right",
        nav_panel("Preferences"),
        nav_panel("Alerts")
    )
)

# --- SERVER DEFINITION ---
server <- function(input, output, session) {
    
    # reactive data storage
    stock_data <- reactiveVal(NULL)
    forecast_results <- reactiveVal(NULL)

    # dashboard logic
    observeEvent(input$analyze, {
        req(input$ticker)
        showNotification(
            "Fetching stock data...",
            type = "message",
            duration = 3
        )

        # fetch data and update reactiveVal
        result <- tryCatch({
            tq_get(
                stringr::str_to_upper(input$ticker),
                get = "stock.prices",
                from = input$date_range[1],
                to = input$date_range[2]
            )
        }, error = function(e) {
            showNotification(paste("Error fetching data for ticker:", input$ticker), type = "error")
            NULL
        })

        stock_data(result)
        forecast_results(NULL)   # reset forecast when new data is fetched
    })

    # render plot
    output$stock_plot <- renderPlotly({
        # check if stock_data() is not NULL
        if (is.null(stock_data())) {
            return(
                plotly_empty(type = "scatter", mode = "markers") %>%
                    layout(
                        title = list(
                            text = "Enter a stock ticker and click 'Analyze' to begin.",
                            y = 0.5
                        ),
                        paper_bgcolor = "#1A2420",
                        plot_bgcolor = "#121A16",
                        font = list(color = "#FFFFFF")
                    )
            )
        }

        plot_ly(
            data = stock_data(),
            x = ~date,
            y = ~adjusted,
            type = "scatter",
            mode = "lines",
            line = list(color = "#39a263")
        ) %>%
        layout(
            paper_bgcolor = "#1A2420",
            plot_bgcolor = "#121A16",
            font = list(color = "#FFFFFF"),
            xaxis = list(
                gridcolor = "rgba(255, 255, 255, 0.1)",
                zerolinecolor = "rgba(255, 255, 255, 0.2)"
            ),
            yaxis = list(
                gridcolor = "rgba(255, 255, 255, 0.1)",
                zerolinecolor = "rgba(255, 255, 255, 0.2)"
            )
        )
    })

    # forecasting logic
    observeEvent(input$generate_forecast, {
        req(stock_data())
        showNotification("Generating forecast, this may take a moment...", type = "message", duration = 5)

        # prepare data for modeling
        ts_data <- ts(stock_data()$adjusted, frequency = 252)   # 252 trading days / yr

        # fit ARIMA model
        fit_arima <- auto.arima(ts_data, stepwise = FALSE, approximation = FALSE)

        # generate forecast
        forecast_arima <- forecast(fit_arima, h = input$forecast_period)

        # manually create a clean df for plotting
        last_date <- last(stock_data()$date)
        forecast_dates <- seq.Date(from = last_date + 1, by = "day", length.out = input$forecast_period)

        forecast_df <- tibble(
            date = forecast_dates,
            point_forecast = as.numeric(forecast_arima$mean),
            lo_95 = as.numeric(forecast_arima$lower[, "95%"]),
            hi_95 = as.numeric(forecast_arima$upper[, "95%"])
        )

        # calculate error metrics
        metrics <- accuracy(forecast_arima) %>% as.data.frame() %>% select(RMSE, MAE, MAPE)

        # store results
        forecast_results(list(
            data = forecast_df,
            metrics = metrics
        ))
    })

    output$forecast_plot <- renderPlotly({
        if (is.null(forecast_results())) {
            return(
                plotly_empty(type = "scatter", mode = "markers") %>%
                layout(
                    title = list(
                        text = "Generate a forecast to see the plot",
                        y = 0.5
                    ),
                    paper_bgcolor = "#1A2420",
                    plot_bgcolor = "#121A16",
                    font = list(
                        color = "#FFFFFF"
                    )
                )
            )
        }

        # build the plot manually with plot_ly
        plot_ly() %>%
            # add historical data
            add_trace(
                data = stock_data(), x = ~date, y = ~adjusted,
                type = "scatter", mode = "lines",
                line = list(color = "rgba(255,255,255,0.7)"),    # light gray for historical data
                name = "Actual"
            ) %>%
            # add forecast data
            add_trace(
                data = forecast_results()$data, x = ~date, y = ~point_forecast,
                type = "scatter", mode = "lines",
                line = list(color = "#39a263"),   # primary green for forecast
                name = "Forecast"
            ) %>%
            # add confidence interval ribbons
            add_ribbons(
                data = forecast_results()$data, x = ~date,
                ymin = ~lo_95, ymax = ~hi_95,
                line = list(color = "transparent"),
                fillcolor = "rgba(57,162,99,0.2)",   # transparent version of primary green
                name = "95% Confidence"
            ) %>%
            layout(
                title = "Stock Price Forecast",
                paper_bgcolor = "#1A2420", plot_bgcolor = "#121A16",
                font = list(color = "#FFFFFF"),
                xaxis = list(title = "Date", gridcolor = "rgba(255,255,255,0.1)"),
                yaxis = list(title = "Adjutsed Price", gridcolor = "rgba(255,255,255,0.1)"),
                legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1)
            )
    })

    output$error_metrics_table <- renderDT({
        req(forecast_results()$metrics)
        datatable(
            forecast_results()$metrics,
            class = "table table-dark table-striped",
            options = list(dom = "t"),
            rownames = FALSE
        )
    })
}

# --- RUN APP ---
shinyApp(ui, server)