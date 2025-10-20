library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(tidyquant)
library(htmltools)

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
            .navbar-brand {
                color: white !important;
            }
            .navbar-nav .nav-link {
                color: white !important;
            }
            .input-group-text {
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

    nav_panel("Forecasting"),
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
    
    stock_data <- reactiveVal(NULL)

    # observe analyze button
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
}

# --- RUN APP ---
shinyApp(ui, server)