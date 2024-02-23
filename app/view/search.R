box::use(
  shiny[
    # element
    div,
    NS,
    uiOutput,
    renderUI,
    moduleServer,
    actionButton,
    reactiveVal,
    observeEvent,
    tags
  ],
  app/logic/data/search_api[search_symbols]
)

#" @export
ui <- function(id) {
  ns <- NS(id)
  div(
    style = "margin-bottom: 10px",
    uiOutput(ns("search_res_ui"))
  )
}

search_res_ui <- function(ns, symbol, description, exchange, is_border) {
  style <- ifelse(
    is_border,
    # "text-align: left; background-color: white; width: 100%; border: none; user-select:none; padding: 6px 12px; border-bottom: 1px solid #ccc;", # nolint
    "display: none",
    "text-align: left; background-color: white; width: 100%; border: none; user-select:none; padding: 6px 12px; border: 1px solid #ccc; border-radius: 4px"
  )
  tags$button(
    paste0(symbol, ": ", description, " (", exchange, ")"),
    style=style,
    onclick=paste0("Shiny.setInputValue(\"app-choose_symbol_btn\", \"", symbol,"\"); document.getElementById(\"app-symbol\").value = \"\"")
  )
}

#" @export
server <- function(id, query, symbol_list, clear=FALSE) {
  moduleServer(id, function(input, output, session) {
    if (clear) {
      output$search_res_ui <- renderUI(div(""))
      return()
    }
    ns <- session$ns
    found <- search_symbols(query)
    if (nrow(found) != 0) {
      output$search_res_ui <- renderUI({
        lapply(seq_len(nrow(found)), function(i) {
          info <- found[i,]
          search_res_ui(ns, info$symbol, info$description, info$exchange, nrow(found) != i)
        })
      })
    } else {
      output$search_res_ui <- renderUI(div(""))
    }
  })
}