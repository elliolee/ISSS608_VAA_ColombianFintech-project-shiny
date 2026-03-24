overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "module-header",
      h2("Customer Overview & Segmentation"),
      p("This module will present customer structure, demographic patterns, acquisition channels, and product adoption.")
    ),
    fluidRow(
      column(
        width = 3,
        div(
          class = "control-card",
          h4("Inputs"),
          p("Placeholder for filters such as customer segment, age group, location, and product type.")
        )
      ),
      column(
        width = 9,
        div(
          class = "output-card",
          h4("Outputs"),
          p("Placeholder for overview charts, KPIs, and segment comparison visualisations.")
        )
      )
    )
  )
}

overview_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
  })
}