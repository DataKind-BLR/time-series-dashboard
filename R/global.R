#' @import shiny

#' @export
launch_app <- function(dataset) {
  n_ <- length(dataset)
  dataset_time <- time(dataset)
  start_date <- lubridate::date_decimal(dataset_time[1])
  end_date <- lubridate::date_decimal(dataset_time[n_])
  
  # go back 10% instances
  train_idx <- floor(0.9 * n_)
  train_date_start <- start_date
  train_date_end <- lubridate::date_decimal(dataset_time[train_idx])
  test_date_start <- lubridate::date_decimal(dataset_time[train_idx + 1])
  test_date_end <- lubridate::date_decimal(dataset_time[n_])
    
  print(paste0("Start Date", start_date))
  print(paste0("End Date", end_date))
  
  app <- list(
    ui = dash_ui(dataset, start_date, end_date, 
                 train_date_start, train_date_end, 
                 test_date_start, test_date_end),
    server = dash_server()
  )

  shiny::runApp(app)
}
