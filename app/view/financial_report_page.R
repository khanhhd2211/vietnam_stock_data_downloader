box::use(
  shiny[
    # element
    div,
    tags,

    # client-server
    NS,
    moduleServer,
    renderTable,
    # renderDataTable,
    tableOutput,
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
    tags$h4("Balance Sheet"),
    dataTableOutput(ns("financial_report")),
  )
}

#' @export
server <- function(id, balance_sheet) {
  moduleServer(id, function(input, output, session) {
    output$financial_report <- renderDataTable(
      formatCurrency(
        datatable(
          balance_sheet,
          rownames = FALSE,
          options = list(
            lengthChange = FALSE,
            scrollX = TRUE,
            searching = FALSE,
            ordering = FALSE,
            pageLength = 25
          ),
        ),
        names(balance_sheet)[2:ncol(balance_sheet)],
        currency = "",
        interval = 3,
        mark = ".",
        digits = 0,
        before = FALSE
      ),
    )
  })
}
