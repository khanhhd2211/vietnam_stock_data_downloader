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
    # renderDataTable,
    tableOutput,
    # dataTableOutput,
    uiOutput,
    renderUI
  ],
  plotly[plot_ly, layout, renderPlotly, plotlyOutput],
  DT[renderDataTable, dataTableOutput],
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
      style = "overflow-x: auto",
      column(
        5,
        style = "overflow-x: auto",
        uiOutput(ns("company_overview_text")),
        tableOutput(ns("company_overview"))
      ),
      column(7, style = "overflow-x: auto", plotlyOutput(ns("candle_plot")))
    ),
    fluidRow(
      column(12,
        style = "overflow-x: auto",
        uiOutput(ns("stock_ohlc_text")),
        dataTableOutput(ns("stock_ohlc"))
      )
    )
  )
}

#" @export
server <- function(id, company_overview, stock_ohlc) {
  moduleServer(id, function(input, output, session) {
    output$company_overview_text <- renderUI({
      tags$h4("Thông tin chung")
    })

    output$company_overview <- renderTable(
      company_overview,
      rownames = TRUE,
      colnames = FALSE
    )

    output$stock_ohlc_text <- renderUI({
      tags$h4("Giá cổ phiếu")
    })
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
                           margin = list(b = 0, l = 20, r = 10, t = 20),
                           xaxis = list(title = "", rangeslider = list(visible = FALSE)))

      fig
    })
  })
}
