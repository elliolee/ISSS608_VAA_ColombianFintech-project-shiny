source("global.R")
source("modules/module_overview.R")
source("modules/module_engagement.R")
source("modules/module_retention.R")

ui <- navbarPage(
  title = "Colombian Fintech Customer Analytics",
  theme = app_theme,
  id = "main_nav",
  
  header = tags$head(
    tags$style(HTML("
      .navbar {
        background: linear-gradient(90deg, #0b1f3a 0%, #123c69 60%, #0f766e 100%) !important;
        border: none;
      }

      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-weight: 600;
      }

      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: transparent !important;
        color: #a7f3d0 !important;
        border-bottom: 2px solid #a7f3d0;
      }

      .app-hero {
        background: linear-gradient(135deg, #0b1f3a 0%, #123c69 45%, #0f766e 100%);
        color: white;
        padding: 2rem 2rem 1.5rem 2rem;
        border-radius: 16px;
        margin-top: 1rem;
        margin-bottom: 1.5rem;
      }

      .app-hero h1 {
        font-weight: 700;
        margin-bottom: 0.5rem;
      }

      .module-card {
        background: white;
        border-radius: 12px;
        padding: 1.2rem;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.06);
        border: 1px solid #e5e7eb;
        margin-bottom: 1rem;
      }

      .module-header h2 {
        font-weight: 700;
        color: #0f172a;
      }

      .module-header p {
        color: #475569;
        margin-bottom: 1rem;
      }

      .control-card {
        background: #f8fafc;
        border-radius: 12px;
        padding: 1rem;
        border: 1px solid #dbeafe;
        min-height: 180px;
      }

      .output-card {
        background: #ffffff;
        border-radius: 12px;
        padding: 1rem;
        border: 1px solid #e5e7eb;
        min-height: 180px;
      }
    "))
  ),
  
  tabPanel(
    "Overview",
    fluidPage(
      div(
        class = "app-hero",
        h1("Colombian Fintech Customer Analytics"),
        p("Interactive visual analytics for customer structure, engagement, and retention.")
      ),
      overview_ui("overview")
    )
  ),
  
  tabPanel(
    "Engagement",
    fluidPage(
      div(
        class = "app-hero",
        h1("Customer Engagement & Experience"),
        p("Explore platform usage, satisfaction, feedback, and service issues.")
      ),
      engagement_ui("engagement")
    )
  ),
  
  tabPanel(
    "Retention",
    fluidPage(
      div(
        class = "app-hero",
        h1("Retention Risk & Customer Value"),
        p("Identify high-risk and high-value customer groups using behavioural and business metrics.")
      ),
      retention_ui("retention")
    )
  )
)

server <- function(input, output, session) {
  overview_server("overview", data = customer_data)
  engagement_server("engagement", data = customer_data)
  retention_server("retention", data = customer_data)
}

shinyApp(ui, server)