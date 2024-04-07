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
    uiOutput, 
    renderUI,

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
  dplyr[rename],
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
    tags$head(tags$meta(name="description", content="Dự án mã nguồn mở được thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và miễn phí. Dự án sử dụng các nguồn cấp dữ liệu đáng tin cậy và không giới hạn của các công ty niêm yết trên sàn chứng khoán Việt Nam.")), # nolint

    # FACEBOOK
    tags$head(tags$meta(property = "og:type", content = "website")),
    tags$head(tags$meta(property = "og:url", content = "https://indicators.scienceforeconomics.com/")), # nolint
    tags$head(tags$meta(property = "og:title", content = "Vietnam Stock Data Downloader")),
    tags$head(tags$meta(property = "og:description", content="Dự án mã nguồn mở được thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và miễn phí. Dự án sử dụng các nguồn cấp dữ liệu đáng tin cậy và không giới hạn của các công ty niêm yết trên sàn chứng khoán Việt Nam.")), # nolint
    tags$head(tags$meta(property = "og:image", content = "static/preview.png")),

    # Twitter
    tags$head(tags$meta(property = "twitter:card", content = "summary_large_image")),
    tags$head(tags$meta(property = "twitter:url", content = "https://indicators.scienceforeconomics.com/")), # nolint
    tags$head(tags$meta(property = "twitter:title", content = "Vietnam Stock Data Downloader")), # nolint
    tags$head(tags$meta(property = "twitter:description", content = "Dự án mã nguồn mở được thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và miễn phí. Dự án sử dụng các nguồn cấp dữ liệu đáng tin cậy và không giới hạn của các công ty niêm yết trên sàn chứng khoán Việt Nam.")), # nolint
    tags$head(tags$meta(property = "twitter:image", content = "static/preview.png")),

    fluidRow(column(width = 12, tags$img(width = "300px", alt = "logo scieco", src = "static/logo.svg"))), # nolint
    div(tags$hr()),
    tags$h3("Tải dữ liệu chứng khoán Việt Nam 💵"),
    fluidRow(column(width = 6, style="margin-top: 10px; margin-bottom: 10px", div("Dự án được thiết kế để tải dữ liệu chứng khoán Việt Nam một cách dễ dàng và hoàn toàn miễn phí."))), # nolint
    fluidRow(
      column(
        width = 6,
        style = "margin-top: 10px; margin-bottom: 10px",
        div(
          "Nguồn dữ liệu: Dữ liệu được lấy từ",
          tags$a(
            href = "https://docs.vnstock.site/",
            "Vnstock - gói phần mềm Python phân tích thị trường chứng khoán Việt Nam."
          ),
          tags$br(),
          "(thinh-vu @ Github, Copyright (c) 2022)." # nolint
        )
      )
    ), # nolint
    fluidRow(column(width = 6, style="margin-bottom: 30px", div("Để sử dụng vui lòng nhập mã cổ phiếu, và khoảng thời gian và ấn tìm kiếm, sau khi có được thông tin, vui lòng ấn tải về để tải dữ liệu mong muốn."))), # nolint
    sidebarLayout(
      # Sidebar with a slider input
      sidebarPanel(
        div(
          textInput(ns("symbol"), "Nhập mã cổ phiếu", placeholder = "Search company"),
          search$ui(ns("search")),
          uiOutput(
            ns("symbol_list"), 
          )
        ),
        dateRangeInput(ns("dates"),
                     "Chọn khoảng thời gian",
                     start = as.character(Sys.Date() - 365),
                     end = as.character(Sys.Date())),
        selectInput(
          ns("fin_report_range"),
          "Chọn loại báo cáo tài chính:",
          c(
            "Theo quý" = "quarterly",
            "Theo năm" = "yearly"
          )
        ),
        actionButton(
          ns("on_search"),
          span("Xem trước", id = "UpdateAnimate", class = ""),
          width = "100%",
          icon = icon("search"),
          class = "btn-primary",
          style = "margin-bottom:14px"
        ),
        selectInput(
          ns("file_type"),
          "Chọn loại tệp tải về:",
          c("CSV" = "csv",
            "Excel" = "excel",
            "Stata" = "stata",
            "SPSS" = "spss")
        ),
        downloadButton(
          ns("downloadData"),
          "Tải xuống tất cả",
          style = "width:100%",
          icon = icon("download")
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(
          tabPanel("Tổng quan", stock_price$ui(ns("stock_price"))),
          tabPanel("Bảng cân đối kế toán", financial_report_page$ui(ns("balance_sheet"))),
          tabPanel("Kế quả hoạt động kinh doanh", financial_report_page$ui(ns("income_statement"))),
          tabPanel("Lưu chuyển dòng tiền", financial_report_page$ui(ns("cash_flow_statement"))),
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
    symbol_list <- reactiveVal(c())

    observeEvent(input$symbol, {
      search$server("search", input$symbol, symbol_list)
    })

    observeEvent(input$choose_symbol_btn, {
      symbol_list(
        unique(
          append(
            symbol_list(),
            input$choose_symbol_btn
          )
        )
      )
      output$symbol_list <- if (length(symbol_list()) > 0) {
        renderUI({
          div(
            div("Danh sách mã cổ phiếu", style = "margin-bottom: 5px; font-weight: 700;"),
            div(
              style = "
                background-color: white;
                padding: 5px 10px;
                border-radius: 4px;
                border: 1px solid #ccc;
                margin-bottom: 10px;
                line-height: 200%;
              ",
              lapply(seq_len(length(symbol_list())), function(i) {
                symbol <- symbol_list()[i]
                tags$span(
                  symbol,
                  style = "
                    background-color: #eee;
                    padding: 2px 5px;
                    border-radius: 4px;
                  "
                )
              })
            )
          )
        })
      }
      search$server("search", input$symbol, symbol_list, clear=TRUE)
    })

    observeEvent(input$on_search, {
        show_modal_progress_line()
        symbol <- symbol_list()[1]
        tryCatch(
          {
            company_overview_df(company_overview(symbol))
            stock_ohlc_df(stock_ohlc(
              symbol,
              start_date = input$dates[1],
              end_date = input$dates[2]
            ))
            update_modal_progress(0.1)
            balance_sheet_df(financial_report(symbol, "balancesheet", input$fin_report_range)) # nolint
            update_modal_progress(0.4)
            income_statement_df(financial_report(symbol, "incomestatement", input$fin_report_range)) # nolint
            update_modal_progress(0.7)
            cash_flow_statement_df(financial_report(symbol, "cashflow", input$fin_report_range)) # nolint
            update_modal_progress(0.9)
            stock_price$server("stock_price", company_overview_df(), stock_ohlc_df())
            financial_report_page$server("balance_sheet", balance_sheet_df()[1:5])
            financial_report_page$server("income_statement", income_statement_df()[1:5])
            financial_report_page$server("cash_flow_statement", cash_flow_statement_df()[1:5])
            update_modal_progress(1)
          },
          error = function(e) {
            report_failure(
              "Có lỗi xảy ra...",
              "Bạn chưa chọn công ty hoặc công ty được chọn không có dữ liệu..."
            )
          }
        )
        remove_modal_progress()
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("output", "_", input$file_type, "_", as.character(Sys.Date()), ".zip")
      },
      content = function(fname) {
        show_modal_spinner()
        tryCatch(
          {
            if (input$file_type == "excel") {
              # EXCEL
              setwd(tempdir())
              for (symbol in symbol_list()) {
                write_xlsx(
                  list(
                    "History" = stock_ohlc(symbol, start_date = input$dates[1], end_date = input$dates[2]),
                    "Company Overview" = rownames_to_column(as.data.frame(company_overview(symbol)), " "), # nolint
                    "Balance Sheet" = financial_report(symbol, "balancesheet", input$fin_report_range),
                    "Income Statement" = financial_report(symbol, "incomestatement", input$fin_report_range),
                    "Cash Flow Statement" = financial_report(symbol, "cashflow", input$fin_report_range)
                  ),
                  paste0(symbol, ".xlsx")
                )
              }
              zip(zipfile = fname, files = paste0(symbol_list(), ".xlsx"), flags = "-q")
            } else if (input$file_type == "csv") {
              # CSV
              setwd(tempdir())
              do.call(file.remove, list(list.files(getwd(), full.names = TRUE)))
              for (symbol in symbol_list()) {
                fs <- c("company_overview.csv", "stock_ohlc.csv", "balance_sheet.csv",
                        "income_statement.csv", "cash_flow.csv")
                fs <- paste(symbol, fs, sep = "_")
                write.csv(rownames_to_column(as.data.frame(company_overview(symbol)), " "), fs[1])
                write.csv(
                  stock_ohlc(symbol, start_date = input$dates[1], end_date = input$dates[2]), 
                  fs[2],
                  row.names = FALSE
                )
                write.csv(
                  financial_report(symbol, "balancesheet", input$fin_report_range),
                  fs[3],
                  row.names = FALSE
                )
                write.csv(
                  financial_report(symbol, "incomestatement", input$fin_report_range),
                  fs[4],
                  row.names = FALSE
                )
                write.csv(
                  financial_report(symbol, "cashflow", input$fin_report_range),
                  fs[5],
                  row.names = FALSE
                )
                zip(zipfile = paste0(symbol, ".zip"), files = fs, flags = "-q")
              }
              zip(zipfile = fname, files = paste0(symbol_list(), ".zip"), flags = "-q")
            } else if (input$file_type == "stata") {
              # STATA
              setwd(tempdir())
              do.call(file.remove, list(list.files(getwd(), full.names = TRUE)))
              for (symbol in symbol_list()) {
                fs <- c("company_overview.csv", "stock_ohlc.dta", "balance_sheet.dta",
                        "income_statement.dta", "cash_flow.dta")
                write.csv(rownames_to_column(as.data.frame(company_overview(symbol)), " "), fs[1])
                write_dta(
                  stock_ohlc(symbol, start_date = input$dates[1], end_date = input$dates[2]),
                  fs[2]
                )
                write_dta(
                  clean_names(financial_report(symbol, "balancesheet", input$fin_report_range)),
                  fs[3]
                )
                write_dta(
                  rename(
                    clean_names(
                      financial_report(symbol, "incomestatement", input$fin_report_range)
                    ),
                    "qu_share_holder_income_growth" = "quarter_share_holder_income_growth"
                  ),
                  fs[4]
                )
                write_dta(
                  clean_names(financial_report(symbol, "cashflow", input$fin_report_range)),
                  fs[5]
                )
                zip(zipfile = paste0(symbol, ".zip"), files = fs, flags = "-q")
              }
              zip(zipfile = fname, files = paste0(symbol_list(), ".zip"), flags = "-q")
            } else if (input$file_type == "spss") {
              # SPSS
              setwd(tempdir())
              do.call(file.remove, list(list.files(getwd(), full.names = TRUE)))
              for (symbol in symbol_list()) {
                fs <- c("company_overview.csv", "stock_ohlc.sav", "balance_sheet.sav",
                        "income_statement.sav", "cash_flow.sav")
                write.csv(rownames_to_column(as.data.frame(company_overview(symbol)), " "), fs[1])
                write_sav(
                  stock_ohlc(symbol, start_date = input$dates[1], end_date = input$dates[2]),
                  fs[2]
                )
                write_sav(
                  clean_names(financial_report(symbol, "balancesheet", input$fin_report_range)),
                  fs[3]
                )
                write_sav(
                  clean_names(financial_report(symbol, "incomestatement", input$fin_report_range)),
                  fs[4]
                )
                write_sav(
                  clean_names(financial_report(symbol, "cashflow", input$fin_report_range)),
                  fs[5]
                )
                zip(zipfile = paste0(symbol, ".zip"), files = fs, flags = "-q")
              }
              zip(zipfile = fname, files = paste0(symbol_list(), ".zip"), flags = "-q")
            }
          },
          error = function(e) {
            print(e)
            report_failure(
              "Có lỗi xảy ra...",
              "Bạn chưa chọn công ty hoặc công ty được chọn không có dữ liệu..."
            )
          }
        )
        remove_modal_spinner()
      },
      contentType = "application/zip"
    )
  })
}
