source("global.R")
source("modules/module_overview.R")
source("modules/module_engagement.R")
source("modules/module_retention.R")

ui <- navbarPage(
  title = "Colombian Fintech Customer Analytics",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0f766e"
  ),
  
  tabPanel(
    "Overview",
    fluidPage(
      br(),
      overview_ui("overview")
    )
  ),
  
  tabPanel(
    "Engagement",
    fluidPage(
      br(),
      engagement_ui("engagement")
    )
  ),
  
  tabPanel(
    "Retention",
    fluidPage(
      br(),
      retention_ui("retention")
    )
  )
)

server <- function(input, output, session) {
  overview_server("overview")
  engagement_server("engagement")
  retention_server("retention")
}

shinyApp(ui, server)