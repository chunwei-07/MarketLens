library(shiny)
library(bslib)
library(tidyverse)
library(plotly)

# UI
ui <- page_navbar(
    title = "MarketLens",
    theme = bs_theme(
        bootswatch = "darkly",
        base_font = font_google("DM Sans"),
        primary = "#39a263",
        bg = "#101214",
        fg = "#FFFFFF",
    ),

    # tabs
    nav_panel("Dashboard"),
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

# server definition
server <- function(input, output, session) {
    # TODO: Server logic here
}

# run app
shinyApp(ui, server)


