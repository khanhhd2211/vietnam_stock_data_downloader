box::use(
  httr,
  jsonlite[fromJSON],
  shiny[renderText, renderTable],
  dplyr[rename, select, `%>%`, mutate_at, mutate, case_when, row_number, full_join],
  tidyr[drop_na],
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

# financial_report <- function(symbol, type, start_date, end_date) {
#   start_date <- as.Date(start_date)
#   start_year <- as.numeric(format(start_date, "%Y"))

#   end_date <- as.Date(end_date)
#   end_year <- as.numeric(format(end_date, "%Y"))
#   # type = bsheet, incsta, cashflow
#   read_year <- function(year) {
#     url <-  paste0(
#       "https://s.cafef.vn/bao-cao-tai-chinh/",
#       symbol,
#       "/",
#       type,
#       "/",
#       year,
#       "/4/1/0/bao-cao-tai-chinh-.chn"
#     )
#     df <- read_html(url) |>
#       html_element("table#tableContent") %>%
#       html_table(na.strings = "") %>%
#       select(X1:X5) %>%
#       drop_na(X1) %>%
#       mutate_at(.vars = 2:5, ~ as.numeric(gsub(",", "", .x)))

#     names(df) <- c("Metrics", paste0("Q", 1:4, ".", year))

#     if (type == "bsheet") {
#       df <- mutate(df, Metrics = case_when(
#         Metrics %in% c("- Nguyên giá", "- Giá trị hao mòn lũy kế") ~
#           paste0(row_number(), Metrics),
#         TRUE ~ Metrics
#       ))
#     }
#     return(df)
#   }

#   if (start_year < end_year) {
#     count_null <- 0
#     current_year <- as.numeric(format(Sys.Date(), "%Y"))
#     if (end_year > current_year) {
#       end_year <- current_year
#     }
#     for (year in end_year:start_year) {
#       if (!exists("df_all")) {
#         df_all <- read_year(year)
#       } else {
#         temp_df <- read_year(year)
#         if (sum(is.na(temp_df[-1])) == nrow(temp_df) * ncol(temp_df[-1])) {
#           count_null <- count_null + 1
#         } else {
#           count_null <- 0
#         }
#         df_all <- df_all %>%
#           full_join(temp_df, by = "Metrics")
#         if (count_null == 3) {
#           break
#         }
#       }
#     }

#     return(df_all)
#   } else if (start_year == end_year) {
#     return(read_year(end_year))
#   } else {
#     return(data.frame())
#   }
# }

#' @export
financial_report <- function(symbol, type, report_range = "quarterly", get_all = TRUE) {
  report_type <- type
  # Map report_range to range (0 for quarterly, 1 for yearly)
  range <- ifelse(report_range == "quarterly", 0, 1)

  # Construct the API URL
  url <- paste0("https://apipubaws.tcbs.com.vn/tcanalysis/v1/finance/", symbol, "/", report_type)

  # Define the query parameters
  query_params <- list(yearly = range, isAll = get_all)

  # Send a GET request and parse the JSON response
  response <- httr$GET(url, query = query_params)

  if (httr$status_code(response) == 200) {
    content <- httr$content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content)

    # Convert data to a data frame
    df <- as.data.frame(data)

    # Convert 'year' and 'quarter' columns to strings
    df$year <- as.character(df$year)
    df$quarter <- as.character(df$quarter)

    # Create an 'index' column based on report_range
    if (report_range == 'yearly') {
      df <- df[, !(names(df) %in% c("quarter"))]
    }
    # else if (report_range == 'quarterly') {
    #   df <- mutate(
    #     df, 
    #     time = paste(df$year, "-Q", df$quarter, sep = ""),
    #     .before = 1
    #   )
    # }

    # Set 'index' as the index and drop 'year' and 'quarter' columns

    return(df)
  } else {
    return(data.frame())
  }
}


# financial_report <- function(symbol, report_type, frequency, headers=ssi_headers) { # nolint
#     # """
#     # This function returns the balance sheet of a stock symbol by a Quarterly or Yearly range.
#     # Args:
#     #     symbol (:obj:`str`, required): 3 digits name of the desired stock.
#     #     report_type (:obj:`str`, required): BalanceSheet, IncomeStatement, CashFlow
#     #     report_range (:obj:`str`, required): Yearly or Quarterly.
#     # """
#     url <- paste0(
#       "https://fiin-fundamental.ssi.com.vn/FinancialStatement/Download",
#       report_type,
#       "?language=vi&OrganCode=",
#       symbol,
#       "&Skip=0&Frequency=",
#       frequency
#     )
#     r <- httr$GET(url, headers)
#     temp_file <- tempfile(fileext = ".xlsx")
#     writeBin(r$content, temp_file)
#     df <- read_excel(temp_file, skip = 7, .name_repair = make.names)
#     df <- rename(df, `Metrics` = 1)
#     df <- df[seq_len(nrow(df) - 3), ]
#     file.remove(temp_file)
#     # df = pd.read_excel(BytesIO(r.content), skiprows=7).dropna()
#     return(df)

# }