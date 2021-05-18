#' Look up the historical exchange rates for any currency pair for the provided time interval
#' @param start_date date
#' @param end_date date
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_numeric
#' @importFrom data.table data.table
#' @importFrom httr GET content
get_exchange_rates <- function( base, symbol, start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0) {
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = "base",
        symbols = "symbol"

      )
    )
    exchange_rates <- content(response)$rates
    rates <- data.table(
      date = as.Date(names(exchange_rates)),
      rate = as.numeric(unlist(exchange_rates)))
    assert_numeric(rates$rate)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhufs(base, symbol,start_date = start_date, end_date = end_date, retried = retried + 1)
  })
  rates
}
