box::use(httr, tidyjson, dplyr)

#' @export
search_symbols <- function(q) {
  url <- paste0("https://api.investing.com/api/search/v2/search?q=", q)
  res <- httr$GET(url)
  if (httr$status_code(res) == 200) {
    search <- httr$content(res)$quotes
    if (length(search) != 0) {
      tidyjson$as.tbl_json(search) |>
        tidyjson$spread_all() |>
        dplyr$filter(flag == "Vietnam", !(description %in% c("VN Index", "VN 30"))) |>
        as.data.frame() |>
        dplyr$select(description, symbol, exchange, flag)
    } else {
      data.frame()
    }
  } else {
    data.frame()
  }
}