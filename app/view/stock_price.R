box::use(
  shiny[
    # element
    div,
    tags,
    fluidRow,
    column,
    mainPanel,

    # client-server
    NS,
    moduleServer,
    renderTable,
    renderDataTable,
    tableOutput,
    dataTableOutput
  ],
  plotly[plot_ly, layout, renderPlotly, plotlyOutput],
  utils[head],
  dplyr[arrange]
)

#" @export
ui <- function(id) {
  ns <- NS(id)
  mainPanel(
    width = 12,
    style = "margin-bottom:25px",
    fluidRow(
      column(5, tags$h4("Company Overview"), tableOutput(ns("company_overview"))),
      column(7, plotlyOutput(ns("candle_plot")))
    ),
    fluidRow(
      column(12,
        tags$h4("Stock Trading Historical Data"),
        dataTableOutput(ns("stock_ohlc"))
      )
    )
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

    output$candle_plot <- renderPlotly({
      fig <- plot_ly(stock_ohlc, x = ~time, type = "candlestick",
                     open = ~open, close = ~close,
                     high = ~high, low = ~low)
      fig <- fig |> layout(title = "",
                           margin = list(b = 0, l = 20),
                           xaxis = list(title = "", rangeslider = list(visible = FALSE)))

      fig
    })
  })
}