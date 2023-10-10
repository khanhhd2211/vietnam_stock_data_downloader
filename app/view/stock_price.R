box::use(
  shiny[
    # element
    icon,
    div,
    tags,

    # client-server
    sliderInput,
    textInput,
    dateRangeInput,
    textOutput,
    renderText,
    actionButton,
    renderPlot,
    NS,
    moduleServer,
    reactive,

    # layout
    fluidPage,
    plotOutput,
    sidebarLayout,
    mainPanel,
    sidebarPanel,
    tabPanel,
    tabsetPanel,
    renderTable,
    renderDataTable,
    tableOutput,
    dataTableOutput
  ],
  app/logic/data/company_overview[company_overview],
  app/logic/data/technical[stock_ohlc],
  utils[head]
)

#" @export
ui <- function(id) {
  ns <- NS(id)
  div(
    tags$h4("Company Overview"),
    tableOutput(ns("company_overview")),
    tags$hr(),
    tags$h4("Stock Trading Historical Data"),
    dataTableOutput(ns("stock_ohlc")),
  )
}

#" @export
server <- function(id, search_symbol, start_date, end_date) {
  moduleServer(id, function(input, output, session) {
    company_overview <- company_overview(search_symbol)

    stock_ohlc <- stock_ohlc(symbol = search_symbol, start_date = start_date, end_date = end_date)

    output$company_overview <- renderTable(
      company_overview,
      rownames = TRUE,
      colnames = FALSE
    )

    output$stock_ohlc <- renderDataTable(
      head(stock_ohlc, 10)
    )
  })
}
