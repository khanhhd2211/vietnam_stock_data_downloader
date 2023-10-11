box::use(
  shiny[
    # element
    div,
    NS,
    uiOutput,
    renderUI,
    moduleServer,
    tags
  ],
  app/logic/data/search_api[search_symbols]
)

#" @export
ui <- function(id) {
  ns <- NS(id)
  div(
    style = "background-color:white; margin-bottom: 15px; border: 1px solid #ccc; border-radius: 4px; overflow:hidden", # nolint
    uiOutput(ns("search_res_ui"))
  )
}

search_res_ui <- function(symbol, description, exchange, is_border) {
  style <- ifelse(
    is_border,
    "background-color:white; user-select:none; padding: 6px 12px; border-bottom: 1px solid #ccc;", # nolint
    "background-color:white; user-select:none; padding: 6px 12px;"
  )
  div(
    style = style,
    paste0(symbol, ": ", description, " (", exchange, ")"),
  )
}

#" @export
server <- function(id, query) {
  moduleServer(id, function(input, output, session) {
    found <- search_symbols(query)
    if (nrow(found) != 0) {
      output$search_res_ui <- renderUI({
        lapply(seq_len(nrow(found)), function(i) {
          info <- found[i,]
          search_res_ui(info$symbol, info$description, info$exchange, nrow(found) != i)
        })
      })
    } else {
      output$search_res_ui <- renderUI(div(""))
    }
  })
}