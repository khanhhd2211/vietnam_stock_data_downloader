box::use(
  httr,
  readxl[read_excel],
  shiny[renderText, renderTable],
  dplyr[rename],
  app/logic/constants/header[ssi_headers]
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

#' @export
financial_report <- function(symbol, report_type, frequency, headers=ssi_headers) { # nolint
    # """
    # This function returns the balance sheet of a stock symbol by a Quarterly or Yearly range.
    # Args:
    #     symbol (:obj:`str`, required): 3 digits name of the desired stock.
    #     report_type (:obj:`str`, required): BalanceSheet, IncomeStatement, CashFlow
    #     report_range (:obj:`str`, required): Yearly or Quarterly.
    # """
    url <- paste0(
      "https://fiin-fundamental.ssi.com.vn/FinancialStatement/Download",
      report_type,
      "?language=vi&OrganCode=",
      symbol,
      "&Skip=0&Frequency=",
      frequency
    )
    r <- httr$GET(url, headers)
    temp_file <- tempfile(fileext = ".xlsx")
    writeBin(r$content, temp_file)
    df <- read_excel(temp_file, skip = 7, .name_repair = make.names)
    df <- rename(df, `Metrics` = 1)
    df <- df[seq_len(nrow(df) - 3), ]
    file.remove(temp_file)
    # df = pd.read_excel(BytesIO(r.content), skiprows=7).dropna()
    return(df)

}