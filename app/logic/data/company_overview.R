box::use(
  httr,
  shiny[renderText, renderTable]
)

#' @export
company_overview <- function(symbol) {
  get_url <- paste0("https://apipubaws.tcbs.com.vn/tcanalysis/v1/ticker/", symbol, "/overview")
  res <- httr$GET(get_url)
  select_cols <- c("ticker", "exchange", "industry", "companyType",
            "noShareholders", "foreignPercent", "outstandingShare", "issueShare",
            "establishedYear", "noEmployees",
            "stockRating", "deltaInWeek", "deltaInMonth", "deltaInYear",
            "shortName", "industryEn", "industryID", "industryIDv2", "website")
  row_names <- c("Ticker", "Exchange", "Industry (VN)", "Company Type",
                 "No Share Holders", "Foreign Percent", "Outstanding Share",
                 "Issue Share", "Established Year", "No Employees", "Stock Rating",
                 "Delta In Week", "Delta In Month", "Delta In Year",
                 "Short Name", "Industry (EN)", "Industry ID", "Industry ID (v2)", "Website")
  if (res$status_code) {
    data.frame(
      Overview = t(data.frame(httr$content(res, type = "application/json")[select_cols])),
      row.names = row_names
    )
  } else {
    "Error"
  }
}