box::use(
  shiny[
    # element
    icon,
    div,

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
    observeEvent,

    # layout
    fluidPage,
    plotOutput,
    sidebarLayout,
    mainPanel,
    sidebarPanel,
    tabPanel,
    tabsetPanel,
  ],
  app/view/stock_price
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    style = "margin-top:20px",
    sidebarLayout(
      # Sidebar with a slider input
      sidebarPanel(
        textInput(ns("symbol"), "Search Symbol", value = "VNM", placeholder = "Search company"),
        dateRangeInput(ns("dates"),
                     "Date range",
                     start = as.character(Sys.Date() - 365),
                     end = as.character(Sys.Date())),
        actionButton(
          ns("on_search"),
          "Search",
          width = "100%",
          icon = icon("search"),
          class = "btn-primary"
        ),
        actionButton(
          ns("download"),
          "Download All",
          width = "100%",
          icon = icon("download"),
          style = "margin-top:14px"
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(
          tabPanel("Stock price", stock_price$ui(ns("stock_price"))),
          tabPanel("Balance Sheet"),
          tabPanel("Income Statement"),
          tabPanel("Cash flow statement"),
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$on_search, {
        stock_price$server("stock_price", input$symbol, input$dates[1], input$dates[2])
    })
  })
}
