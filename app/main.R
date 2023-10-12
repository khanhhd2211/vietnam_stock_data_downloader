options(scipen = 20)

box::use(
  shiny[
    # element
    icon,
    tags,
    div,
    span,

    # client-server
    sliderInput,
    textInput,
    selectInput,
    dateRangeInput,
    textOutput,
    renderText,
    actionButton,
    renderPlot,
    NS,
    moduleServer,
    reactive,
    observeEvent,
    reactiveVal,
    downloadButton,
    conditionalPanel,

    # layout
    fluidPage,
    fluidRow,
    column,
    plotOutput,
    sidebarLayout,
    mainPanel,
    sidebarPanel,
    tabPanel,
    tabsetPanel,

    # utils
    downloadHandler
  ],
  shinybusy[
    show_modal_progress_line,
    update_modal_progress,
    remove_modal_progress,
    show_modal_spinner,
    remove_modal_spinner,
    report_failure
  ],
  janitor[clean_names],
  utils[write.csv, zip],
  writexl[write_xlsx],
  tibble[rownames_to_column],
  haven[write_dta, write_sav],
  app/view/search,
  app/view/stock_price,
  app/view/financial_report_page,
  app/logic/data/fundamental[company_overview, financial_report],
  app/logic/data/technical[stock_ohlc]
)

#" @export
ui <- function(id) { # nolint
  ns <- NS(id)
  fluidPage(
    style = "margin-top:20px; margin-right:0px; margin-bottom:50px",
    tags$head(tags$title("Vietnam Stock Data Downloader")),
    tags$head(tags$meta(name="description", content="Đây là website được SciEco thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và miễn phí. SciEco sử dụng các nguồn cấp dữ liệu đáng tin cậy và không giới hạn từ công ty chứng khoán và công ty phân tích thị trường tại Việt Nam.")), # nolint

    # FACEBOOK
    tags$head(tags$meta(property = "og:type", content = "website")),
    tags$head(tags$meta(property = "og:url", content = "https://vnstock.scienceforeconomics.com/")),
    tags$head(tags$meta(property = "og:title", content = "[SciEco] Vietnam Stock Data Downloader")),
    tags$head(tags$meta(property = "og:description", content="Website được SciEco thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và miễn phí. SciEco sử dụng các nguồn cấp dữ liệu đáng tin cậy và không giới hạn từ công ty chứng khoán và công ty phân tích thị trường tại Việt Nam")), # nolint
    tags$head(tags$meta(property = "og:image", content = "/static/preview.png")),

    # Twitter
    tags$head(tags$meta(property = "twitter:card", content = "summary_large_image")),
    tags$head(tags$meta(property = "twitter:url", content = "https://vnstock.scienceforeconomics.com/")), # nolint
    tags$head(tags$meta(property = "twitter:title", content = "[SciEco] Vietnam Stock Data Downloader")), # nolint
    tags$head(tags$meta(property = "twitter:description", content = "Website được SciEco thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và miễn phí. SciEco sử dụng các nguồn cấp dữ liệu đáng tin cậy và không giới hạn từ công ty chứng khoán và công ty phân tích thị trường tại Việt Nam")), # nolint
    tags$head(tags$meta(property = "twitter:image", content = "/static/preview.png")),

    fluidRow(column(width = 12, tags$img(width = "300px", alt = "logo scieco", src = "/static/logo.svg"))), # nolint
    tags$hr(),
    tags$h3("Tải dữ liệu chứng khoán Việt Nam 💵"),
    fluidRow(column(width = 6, style="margin-top: 10px; margin-bottom: 10px", div("Đây là website được SciEco thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và miễn phí. SciEco sử dụng các nguồn cấp dữ liệu đáng tin cậy và không giới hạn từ công ty chứng khoán và công ty phân tích thị trường tại Việt Nam."))), # nolint
    fluidRow(column(width = 6, style="margin-bottom: 30px", div("Để sử dụng vui lòng nhập mã cổ phiếu, và khoảng thời gian và ấn tìm kiếm, sau khi có được thông tin, vui lòng ấn tải về để tải dữ liệu mong muốn."))), # nolint
    sidebarLayout(
      # Sidebar with a slider input
      sidebarPanel(
        div(
          textInput(ns("symbol"), "Search Symbol",
                    value = "VNM", placeholder = "Search company"),
          search$ui(ns("search"))
        ),
        dateRangeInput(ns("dates"),
                     "Date range",
                     start = as.character(Sys.Date() - 365),
                     end = as.character(Sys.Date())),
        actionButton(
          ns("on_search"),
          span("Search", id = "UpdateAnimate", class = ""),
          width = "100%",
          icon = icon("search"),
          class = "btn-primary",
          style = "margin-bottom:14px"
        ),
        selectInput(
          ns("file_type"),
          "Download File Type:",
          c("CSV" = "csv",
            "Excel" = "excel",
            "Stata" = "stata",
            "SPSS" = "spss")
        ),
        downloadButton(
          ns("downloadData"),
          "Download All",
          style = "width:100%",
          icon = icon("download")
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(
          tabPanel("Stock price", stock_price$ui(ns("stock_price"))),
          tabPanel("Balance Sheet", financial_report_page$ui(ns("balance_sheet"))),
          tabPanel("Income Statement", financial_report_page$ui(ns("income_statement"))),
          tabPanel("Cash Flow Statement", financial_report_page$ui(ns("cash_flow_statement"))),
        )
      )
    )
  )
}

#" @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    company_overview_df <- reactiveVal()
    stock_ohlc_df <- reactiveVal()
    balance_sheet_df <- reactiveVal()
    income_statement_df <- reactiveVal()
    cash_flow_statement_df <- reactiveVal()

    observeEvent(input$symbol, {
      search$server("search", input$symbol)
    })

    observeEvent(input$on_search, {
        show_modal_progress_line()
        tryCatch(
          {
            company_overview_df(company_overview(input$symbol))
            stock_ohlc_df(stock_ohlc(
              input$symbol,
              start_date = input$dates[1],
              end_date = input$dates[2]
            ))
            update_modal_progress(0.1)
            balance_sheet_df(financial_report(input$symbol, "bsheet", input$dates[1], input$dates[2])) # nolint
            update_modal_progress(0.4)
            income_statement_df(financial_report(input$symbol, "incsta", input$dates[1], input$dates[2])) # nolint
            update_modal_progress(0.7)
            cash_flow_statement_df(financial_report(input$symbol, "cashflow", input$dates[1], input$dates[2])) # nolint
            update_modal_progress(0.9)
            stock_price$server("stock_price", company_overview_df(), stock_ohlc_df())
            financial_report_page$server("balance_sheet", balance_sheet_df()[1:5])
            financial_report_page$server("income_statement", income_statement_df()[1:5])
            financial_report_page$server("cash_flow_statement", cash_flow_statement_df()[1:5])
            update_modal_progress(1)
          },
          error = function(e) {
            report_failure(
              "Oups...",
              "Something went wrong"
            )
          }
        )
        remove_modal_progress()
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$file_type == "excel") {
          paste0(input$symbol, "_", "output", "_", as.character(Sys.Date()), ".xlsx")
        } else {
          paste0(
            input$symbol, "_", "output", "_", 
            input$file_type, "_", as.character(Sys.Date()), ".zip"
          )
        }
      },
      content = function(fname) {
        show_modal_spinner()
        tryCatch(
          {
            # balance_sheet_df(financial_report(input$symbol, "bsheet", input$dates[1], input$dates[2])) # nolint
            # income_statement_df(financial_report(input$symbol, "incsta", input$dates[1], input$dates[2])) # nolint
            # cash_flow_statement_df(financial_report(input$symbol, "cashflow", input$dates[1], input$dates[2])) # nolint
            if (input$file_type == "excel") {
              # EXCEL
              write_xlsx(
                list(
                  "History" = stock_ohlc_df(),
                  "Company Overview" = rownames_to_column(as.data.frame(company_overview_df()), " "), # nolint
                  "Balance Sheet" = balance_sheet_df(),
                  "Income Statement" = income_statement_df(),
                  "Cash Flow Statement" = cash_flow_statement_df()
                ),
                fname,
              )
            } else if (input$file_type == "csv") {
              # CSV
              fs <- c("company_overview.csv", "stock_ohlc.csv", "balance_sheet.csv",
                      "income_statement.csv", "cash_flow.csv")
              tmpdir <- tempdir()
              setwd(tempdir())
              write.csv(company_overview_df(), fs[1])
              write.csv(stock_ohlc_df(), fs[2], row.names = FALSE)
              write.csv(balance_sheet_df(), fs[3], row.names = FALSE)
              write.csv(income_statement_df(), fs[4], row.names = FALSE)
              write.csv(cash_flow_statement_df(), fs[5], row.names = FALSE)
              zip(zipfile = fname, files = fs, flags = "-q")
            } else if (input$file_type == "stata") {
              # STATA
              fs <- c("company_overview.csv", "stock_ohlc.dta", "balance_sheet.dta",
                      "income_statement.dta", "cash_flow.dta")
              tmpdir <- tempdir()
              setwd(tempdir())
              write.csv(company_overview_df(), fs[1])
              write_dta(stock_ohlc_df(), fs[2])
              write_dta(clean_names(balance_sheet_df()), fs[3])
              write_dta(clean_names(income_statement_df()), fs[4])
              write_dta(clean_names(cash_flow_statement_df()), fs[5])
              zip(zipfile = fname, files = fs, flags = "-q")
            } else if (input$file_type == "spss") {
              # SPSS
              fs <- c("company_overview.csv", "stock_ohlc.sav", "balance_sheet.sav",
                      "income_statement.sav", "cash_flow.sav")
              tmpdir <- tempdir()
              setwd(tempdir())
              write.csv(company_overview_df(), fs[1])
              write_sav(stock_ohlc_df(), fs[2])
              write_sav(clean_names(balance_sheet_df()), fs[3])
              write_sav(clean_names(income_statement_df()), fs[4])
              write_sav(clean_names(cash_flow_statement_df()), fs[5])
              zip(zipfile = fname, files = fs, flags = "-q")
            }
          },
          error = function(e) {
              report_failure(
                "Oups...",
                "Something went wrong"
              )
          }
        )
        remove_modal_spinner()
      },
      contentType = "application/zip"
    )
  })
}
