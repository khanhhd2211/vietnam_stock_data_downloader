box::use(
  app/logic/constants/header[entrade_headers]
)

#' @export
stock_ohlc <- function(symbol, start_date="2023-06-01", end_date="2023-06-17", resolution="1D", type="stock", headers=entrade_headers) { # nolint # DNSE source (will be published on vnstock)
  # """
  # Get historical price data from entrade.com.vn. The unit price is VND.
  # Parameters:
  #     symbol (str): ticker of a stock or index. Available indices are: VNINDEX, VN30, HNX, HNX30, UPCOM, VNXALLSHARE, VN30F1M, VN30F2M, VN30F1Q, VN30F2Q # nolint
  #     from_date (str): start date of the historical price data
  #     to_date (str): end date of the historical price data
  #     resolution (str): resolution of the historical price data. Default is '1D' (daily), other options are '1' (1 minute), 15 (15 minutes), 30 (30 minutes), '1H' (hourly) # nolint
  #     type (str): stock or index. Default is 'stock'
  #     headers (dict): headers of the request
  # Returns:
  #     :obj:`pandas.DataFrame`:
  #     | time | open | high | low | close | volume |
  #     | ----------- | ---- | ---- | --- | ----- | ------ |
  #     | YYYY-mm-dd  | xxxx | xxxx | xxx | xxxxx | xxxxxx |
  # """
  # add one more day to end_date
  end_date <- as.Date(end_date) + 1
  # convert from_date, to_date to timestamp
  day_to_sec <- 24 * 60 * 60
  from_timestamp <- as.numeric(as.Date(start_date)) * day_to_sec
  to_timestamp <- as.numeric(end_date) * day_to_sec

  url <- paste0(
    "https://services.entrade.com.vn/chart-api/v2/ohlcs/",
    type,
    "?from=",
    from_timestamp,
    "&to=",
    to_timestamp,
    "&symbol=",
    symbol,
    "&resolution=",
    resolution
  )
  response <- httr::GET(url, headers)

  if (httr::status_code(response) == 200) {
    df <- httr::content(response)

    df <- data.frame(
      time = as.Date(as.POSIXct(as.numeric(df$t), origin = as.Date("1970-01-01"))),
      open = as.numeric(df$o),
      high = as.numeric(df$h),
      low = as.numeric(df$l),
      close = as.numeric(df$c),
      volume = as.numeric(df$v)
    )

    return(df)
  }
}
