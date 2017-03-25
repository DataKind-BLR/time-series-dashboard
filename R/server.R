dash_server <- function() {
  shinyServer(function(input, output) {
    getData <- reactive({
      raw <- dataset
      dataStart <-
        c(lubridate::year(input$dataRange[1]), lubridate::month(input$dataRange[1]))
      dataEnd <-
        c(lubridate::year(input$dataRange[2]), lubridate::month(input$dataRange[2]))
      return(window(raw, start = dataStart, end = dataEnd))
    })
    
    rValues <- reactiveValues()
    
    getProcessedWithoutStl <- reactive({
      monthly <- getData()
      if (input$cleanOutliers) {
        monthly <- forecast::tsclean(monthly)
      }
      monthly
    })
    
    getProcessed <- reactive({
      monthly <- getData()
      if (input$cleanOutliers) {
        monthly <- forecast::tsclean(monthly)
      }
      
      if (input$STL) {
        x.window <- "periodic"
        if (input$STL.Window > 0) {
          x.window <- input$STL.Window
        }
        stl.fit <- stl(monthly, x.window)
        monthly <- forecast::seasadj(stl.fit)
        rValues$stl.fit <- stl.fit
      }
      
      monthly
    })
    
    output$plotOutliers <- renderPlot({
      series <- getData()
      plot(series, col = "red", lty = 2)
      lines(forecast::tsclean(series), lty = 1)
      legend(
        "topright",
        col = c("red", "black"),
        lty = c(2, 1),
        legend = c("Original", "Cleaned")
      )
    })
    
    output$plotSTL <- renderPlot({
      series <- getData()
      if (input$cleanOutliers) {
        series <- forecast::tsclean(series)
      }
      x.window <- "periodic"
      if (input$STL.Window > 0) {
        x.window <- input$STL.Window
      }
      plot(stl(series, x.window))
    })
    
    output$plotTs <- renderPlot({
      forecast::tsdisplay(getProcessed(), main = "Processed Data")
    })
    
    output$plotForecast <- renderPlot({
      monthly <- getProcessedWithoutStl()
      
      trainStart <- c(lubridate::year(input$trainRange[1]), lubridate::month(input$trainRange[1]))
      trainEnd <- c(lubridate::year(input$trainRange[2]), lubridate::month(input$trainRange[2]))
      testStart <- c(lubridate::year(input$testRange[1]), lubridate::month(input$testRange[1]))
      testEnd <- c(lubridate::year(input$testRange[2]), lubridate::month(input$testRange[2]))
      print(trainStart)
      print(trainEnd)
      print(testStart)
      print(testEnd)
      
      # split into 'test' and 'train' set
      trainData <- window(monthly, start = trainStart, end = trainEnd)
      testData <- window(monthly, start = testStart, end = testEnd)
      rValues$trainData <- trainData
      rValues$testData <- testData
      
      stl.period <- "periodic"
      if (input$STL.Window > 0) {
        stl.period <- input$STL.Window
      }
      
      ts_model <- input$method
      
      if(ts_model == "ARIMA") {
        order <- c(input$ARIMA.p, input$ARIMA.d, input$ARIMA.q)
        model_function <- function(trainData) {
          forecast::Arima(trainData, order = order)
        }
        
        if(input$STL) {
          model <- forecast::stlm(trainData,
                                  s.window = stl.period,
                                  modelfunction = model_function)
        } else {
          model <- model_function(trainData)
        }
      } else if (ts_model == "Exponential Smoothing - ETS") {
        damped <- input$damping
        ets.model <- paste0(
          substr(input$error, 1, 1),
          substr(input$trend, 1, 1),
          substr(input$seasonality, 1, 1),
          collapse = ""
        )
        
        model_function <- function(y) {
          min_ts_value <- min(y)
          bias_value <- (-1 * min_ts_value) + 1
          ES_series <- y + bias_value
          ES_series[ES_series == 0] = 0.1
          forecast::ets(ES_series,
                        model = ets.model,
                        damped = damped)
        }
        
        if(input$STL) {
          model <- forecast::stlm(trainData,
                                  s.window = stl.period,
                                  modelfunction = model_function)
        } else {
          model <- model_function(trainData)
        }
      }
      
      forecasted <- forecast::forecast(model,
                                       h=length(testData))
      rValues$forecasted <- forecasted
      forecast::plot.forecast(forecasted,
                    main = paste0("Forecasts for using ", input$method))
      lines(testData, lty = 2, col = "red")
    })
    
    output$displayMetrics <- renderTable({
      forecast::accuracy(rValues$forecasted, rValues$testData)
    })
  })
}
