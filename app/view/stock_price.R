box::use(
  shiny[
    # element
    div,
    tags,

    # client-server
    NS,
    moduleServer,
    renderTable,
    renderDataTable,
    tableOutput,
    dataTableOutput
  ],
  utils[head],
  dplyr[arrange]
)

#" @export
ui <- function(id) {
  ns <- NS(id)
  div(
    style = "margin-bottom:25px",
    tags$h4("Company Overview"),
    tableOutput(ns("company_overview")),
    tags$hr(),
    tags$h4("Stock Trading Historical Data"),
    dataTableOutput(ns("stock_ohlc")),
  )
}

#" @export
server <- function(id, company_overview, stock_ohlc) {
  moduleServer(id, function(input, output, session) {
    output$company_overview <- renderTable(
      company_overview,
      rownames = TRUE,
      colnames = FALSE
    )

    output$stock_ohlc <- renderDataTable(
      arrange(stock_ohlc, desc(time)),
      options = list(
        lengthChange = FALSE,
        pageLength = 10,
        paging = TRUE,
        searching = FALSE,
        ordering = FALSE
        )
    )
  })
}
