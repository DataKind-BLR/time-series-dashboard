
dash_ui <- function(dataset, start_date, end_date,
                    train_date_start, train_date_end, 
                    test_date_start, test_date_end) {
  shinyUI(fluidPage(
    theme = shinythemes::shinytheme('cosmo'),
    titlePanel("DataKind-BLR Time Series Forecasting Tool"),
    sidebarLayout(sidebarPanel(
      width = 4,
      tabsetPanel(
        tabPanel(
          title = 'Data and pre-processing',
          fluidRow(
            h5("Data Preprocessing Controls"),
            checkboxInput("cleanOutliers", "Remove Outliers", value = F),
            checkboxInput("STL", "STL", value = F),
            sliderInput(
              "STL.Window",
              label = "STL Window (0=periodic)",
              min = 0,
              max = 12,
              value = 0
            )
          ),
          fluidRow(
            dateRangeInput(
              "dataRange",
              "DataSet Range",
              start  = start_date,
              end    = end_date,
              format = "dd-mm-yyyy",
              separator = " to ",
              startview = "year"
            ),
            dateRangeInput(
              "trainRange",
              "Training Set Range",
              start  = train_date_start,
              end    = train_date_end,
              format = "dd-mm-yyyy",
              separator = " to ",
              startview = "year"
            ),
            dateRangeInput(
              "testRange",
              "Test Set Range",
              start  = test_date_start,
              end    = test_date_end,
              format = "dd-mm-yyyy",
              separator = " to ",
              startview = "year"
            )
          )
        ),
        tabPanel(
          title = "Forecasting Method",
          fluidRow(selectInput(
            "method",
            label = h5("Modeling Method"),
            choices = c("ARIMA",
                        "Exponential Smoothing - ETS")
          )),
          fluidRow(
            h3("ARIMA Parameters"),
            sliderInput(
              "ARIMA.p",
              label = "p",
              min = 0,
              max = 18,
              value = 1
            ),
            sliderInput(
              "ARIMA.d",
              label = "d",
              min = 0,
              max = 12,
              value = 0
            ),
            sliderInput(
              "ARIMA.q",
              label = "q",
              min = 0,
              max = 18,
              value = 1
            )
          ),
          fluidRow(
            h3("ETS Parameters"),
            h5("Select the type of ETS model:"),
            selectInput('error', 'Error', c("Additive", "Multiplicative")),
            selectInput('trend', 'Trend', c("None", "Additive", "Multiplicative")),
            selectInput(
              'seasonality',
              'Seasonality',
              c("None", "Additive", "Multiplicative")
            ),
            h5('Tick if damping is required for trend:'),
            checkboxInput("damping", "Damped Trend", value = F),
            h5(''),
            h5(''),
            h5('Note: No output plot will be displayed for incompatible model')
          )
        )
      )
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        title = 'Data Pre-processing',
        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),
        plotOutput("plotOutliers"),
        plotOutput("plotSTL")
      ),
      tabPanel(
        title = 'Forecasts / Results',
        plotOutput("plotTs"),
        plotOutput("plotForecast"),
        tableOutput("displayMetrics")
      )
    )))
  ))
}
