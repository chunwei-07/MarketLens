# 📈 MarketLens: AI-Powered Stock Analysis Dashboard

<center>

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-007BC2?style=for-the-badge&logo=rstudio&logoColor=white)
![Plotly](https://img.shields.io/badge/Plotly-3F4F75?style=for-the-badge&logo=plotly&logoColor=white)
![Google Gemini](https://img.shields.io/badge/Google%20Gemini-8E75B2?style=for-the-badge&logo=googlebard&logoColor=white)

</center>

**MarketLens** is an interactive stock market forecasting and analysis application built with **R** and **Shiny**. It combines real-time financial data with time-series forecastings models (ARIMA & Prophet) and Generative AI (Google Gemini) to provide actionable investment insights.

## ✨ Key Features
- 📊 **Interactive Dashboard:** Live stock data visualisation using `plotly` and `tidyquant` (Yahoo Finance API).
- 🔮 **Advanced Forecasting:**
  - Compare **ARIMA** vs. **Prophet** models side-by-side.
  - Visualise prediction intervals (95% confidence).
  - View detailed error metrics (RMSE, MAE, MAPE) with explanatory tooltips.
- 🤖 **AI Analyst Insights:** Integrated **Google Gemini 2.5 Pro** to generate natural language summaries of forecast trends and model confidence.
- 📉 **Technical Analysis:** Deep dive with interactive Candlestick charts, SMA (50/200), RSI, and MACD indicators.
- 🌗 **Dynamic UI:** Modern, responsive interface built with `bslib` featuring a toggleable **Dark/Light mode**.
- 📄 **PDF Reporting:** Generate and download professional PDF analysis reports using `rmarkdown`.

## 📸 Screenshots
Dashboard Light  |    Dashboard Dark
:-------------:|:-------------:
![Light Mode Dashboard](screenshots/dashboard_light.png) | ![Dark Mode Dashboard](screenshots/dashboard_dark.png)

<center> 

**Forecasting with AI** 

</center>

![Forecasting with AI](screenshots/forecasting_with_ai.png)

<center> 

**Technical Analysis**

</center>

![Technical Analysis](screenshots/technical_analysis.png)

PDF Report Pg 1 | PDF Report Pg 2
:--------------:|:---------------: 
![PDF Page 1](screenshots/pdf_pg1.png) | ![PDF Page 2](screenshots/pdf_pg2.png)


## 🛠️ Tech Stack
- **Core**: R, Shiny, bslib (UI)
- **Data & Manipulation:** tidyverse, tidyquant, lubridate
- **Visualisation:** plotly, DT(DataTables)
- **Modeling:** forecast (ARIMA), prophet
- **AI Integration:** httr2 (Google Gemini API)
- **Reporting:** rmarkdown, knitr, tinytex

## 🚀 Installation & Setup
Follow these steps to run MarketLens locally.

### 1. Prerequisites
Ensure you have the following installed:
- [R](https://cran.r-project.org/) (Version 4.0 or higher)
- [VS Code](https://code.visualstudio.com/) or RStudio
- **Pandoc**: Required for generating PDF reports.
  - [Download Pandoc here](https://pandoc.org/installing.html).
  - *Note: Restart your IDE after installing Pandoc.*

### 2. Clone the Repository
```
git clone https://github.com/chunwei-07/MarketLens.git
cd MarketLens
```

### 3. Install R Packages
Open your R console and run:
```
install.packages(c(
  "shiny", "bslib", "tidyverse", "plotly", 
  "tidyquant", "htmltools", "forecast", 
  "DT", "prophet", "rmarkdown", "httr2", "glue"
))
```

### 4. Set Up API Keys (Important!)🔑
This app uses the **Google Gemini API** for AI insights. You need a free API key.
1. Get your key from [Google AI Studio](https://aistudio.google.com/app/api-keys).
2. Create a new file in the project root named `.Renviron`.
3. Add your key to the file:
```
GEMINI_API_KEY=AIzaSy...[Your Actual Key]...
```
4. **Security Note:** The `.Renviron` file is included in `.gitignore` to prevent it from being uploaded to GitHub.

### 5. Run the App
You can launch the app directly from the R Terminal:
```
shiny::runApp('app.R')
```

## 📂 Project Structure
```
MarketLens/
├── app.R                 # Main application logic (UI & Server)
├── report_template.Rmd   # Template for generating PDF reports
├── .Renviron             # API keys (Hidden/Ignored by Git)
├── .gitignore            # Git ignore rules
└── README.md             # Documentation
```

## 🤝 Contributing
Contributions, issues, and feature requests are welcome! Feel free to fork the repository and submit a pull request.

## 📜 License
This project is open-source and available under the **MIT License**.

---

<center>

*Built by chunwei-07*

</center>

---
