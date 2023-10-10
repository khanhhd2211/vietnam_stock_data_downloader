box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    uiOutput(ns("message"))
  )
}

