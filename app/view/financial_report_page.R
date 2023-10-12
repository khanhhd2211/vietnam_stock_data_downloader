box::use(
  shiny[
    # element
    div,
    tags,
    column,

    # client-server
    NS,
    moduleServer,
    renderTable,
    # renderDataTable,
    tableOutput,
    uiOutput,
    renderUI
    # dataTableOutput
  ],
  DT[datatable, formatCurrency, renderDataTable, dataTableOutput],
  utils[head]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    style = "margin-bottom:25px",
    class = "financial_report",
    uiOutput(ns("financial_report_text")),
    dataTableOutput(ns("financial_report")),
  )
}

#' @export
server <- function(id, report) {
  moduleServer(id, function(input, output, session) {
    output$financial_report_text <- renderUI({
      column(
        width = 8,
        style = "padding: 0; margin-top: 25px; margin-bottom:25px;",
        tags$b("Lưu ý"),
        ": bảng bên dưới chỉ hiển thị năm cuối cùng trong khoảng thời gian được tìm kiếm, để lấy dữ liệu đầy đủ các năm, vui lòng ấn tải về.") # nolint
    })

    output$financial_report <- renderDataTable(
      formatCurrency(
        datatable(
          report,
          rownames = FALSE,
          options = list(
            lengthChange = FALSE,
            scrollX = TRUE,
            searching = FALSE,
            ordering = FALSE,
            pageLength = 25
          ),
        ),
        names(report)[2:ncol(report)],
        currency = "",
        interval = 3,
        mark = ".",
        digits = 0,
        before = FALSE
      ),
    )
  })
}
