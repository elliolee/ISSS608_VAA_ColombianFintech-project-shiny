library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)
library(scales)
library(heatmaply)
library(viridis)
library(bslib)

customer_data <- read.csv("../data_clean/customer_data_cleaned.csv") %>%
  mutate(
    # Convert TRUE/FALSE product columns to 0/1
    savings_account    = as.integer(savings_account),
    credit_card        = as.integer(credit_card),
    personal_loan      = as.integer(personal_loan),
    investment_account = as.integer(investment_account),
    insurance_product  = as.integer(insurance_product),
    # Factor conversions
    gender              = as.factor(gender),
    customer_segment    = as.factor(customer_segment),
    acquisition_channel = as.factor(acquisition_channel),
    income_bracket      = factor(income_bracket,
                                 levels = c("Very Low","Low","Medium",
                                            "High","Very High")),
    city                = as.factor(city),
    age_group = cut(age,
                    breaks = c(18,25,35,45,55,70),
                    labels = c("18-25","26-35","36-45","46-55","55+"),
                    include.lowest = TRUE),
    active_products = savings_account + credit_card +
      personal_loan + investment_account +
      insurance_product
  )

seg_col <- c("inactive"   = "#7f9baa",   # muted blue-grey
             "occasional" = "#4db8b8",   # teal (matches navbar)
             "power"      = "#1a6b6b",   # deep teal
             "regular"    = "#a8d5d5")   # light teal


overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # KPI Row
    fluidRow(
      column(width = 2, div(class = "kpi-card",
                            div(class = "kpi-val", textOutput(ns("kpi_total"))),
                            div(class = "kpi-lbl", "Total Customers"))),
      column(width = 2, div(class = "kpi-card",
                            div(class = "kpi-val", textOutput(ns("kpi_prod"))),
                            div(class = "kpi-lbl", "Avg Active Products"))),
      column(width = 2, div(class = "kpi-card",
                            div(class = "kpi-val", textOutput(ns("kpi_seg"))),
                            div(class = "kpi-lbl", "Dominant Segment"))),
      column(width = 2, div(class = "kpi-card",
                            div(class = "kpi-val", textOutput(ns("kpi_age"))),
                            div(class = "kpi-lbl", "Avg Age"))),
      column(width = 2, div(class = "kpi-card",
                            div(class = "kpi-val", textOutput(ns("kpi_ch"))),
                            div(class = "kpi-lbl", "Top Channel"))),
      column(width = 2, div(class = "kpi-card",
                            div(class = "kpi-val", textOutput(ns("kpi_freq"))),
                            div(class = "kpi-lbl", "Avg Tx Frequency")))
    ),
    
    br(),
    
    fluidRow(
      column(
        width = 3,
        div(
          class = "control-card",
          h4("Inputs"),
          selectInput(ns("seg"),     "Customer Segment",
                      c("All", "inactive","occasional","regular","power")),
          selectInput(ns("gender"),  "Gender",
                      c("All", "Male", "Female")),
          selectInput(ns("income"),  "Income Bracket",
                      c("All","Very Low","Low","Medium","High","Very High")),
          selectInput(ns("channel"), "Acquisition Channel",
                      c("All")),
          selectInput(ns("city"),    "City",
                      c("All")),
          hr(),
          h5("Product Owned"),
          checkboxInput(ns("sav"), "Savings Account",    FALSE),
          checkboxInput(ns("crd"), "Credit Card",        FALSE),
          checkboxInput(ns("ln"),  "Personal Loan",      FALSE),
          checkboxInput(ns("inv"), "Investment Account", FALSE),
          checkboxInput(ns("ins"), "Insurance Product",  FALSE)
        )
      ),
      column(
        width = 9,
        div(
          class = "output-card",
          h4("Outputs"),
          tabsetPanel(
            tabPanel("Segment by Age",
                     br(),
                     fluidRow(
                       column(6, plotlyOutput(ns("age_seg"), height = "370px")),
                       column(6, plotlyOutput(ns("txfreq"),  height = "370px"))
                     )
            ),
            tabPanel("Behaviour Analysis",
                     br(),
                     fluidRow(
                       column(8, plotlyOutput(ns("scatter"),      height = "390px")),
                       column(4, plotlyOutput(ns("channel_plot"), height = "390px"))
                     ),
                     br(),
                     fluidRow(
                       column(6, plotlyOutput(ns("gender_seg"), height = "310px")),
                       column(6, plotlyOutput(ns("income_seg"), height = "310px"))
                     )
            ),
            tabPanel("Product Adoption",
                     br(),
                     fluidRow(
                       column(8, plotlyOutput(ns("product"),     height = "390px")),
                       column(4, plotlyOutput(ns("active_prod"), height = "390px"))
                     )
            ),
            tabPanel("City Heatmap",
                     br(),
                     plotlyOutput(ns("heatmap"), height = "570px")
            ),
            tabPanel("Explorer Table",
                     br(),
                     DTOutput(ns("table"))
            )
          )
        )
      )
    )
  )
}

overview_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    seg_col <- c("inactive"   = "#7f9baa",
                 "occasional" = "#4db8b8",
                 "power"      = "#1a6b6b",
                 "regular"    = "#a8d5d5")
    
    # Populate dynamic choices
    observe({
      updateSelectInput(session, "channel",
                        choices = c("All", levels(data$acquisition_channel)))
      updateSelectInput(session, "city",
                        choices = c("All", levels(data$city)))
    })
    
    # Filtered data
    df <- reactive({
      d <- data
      if (input$seg     != "All") d <- d %>% filter(customer_segment    == input$seg)
      if (input$gender  != "All") d <- d %>% filter(gender              == input$gender)
      if (input$income  != "All") d <- d %>% filter(income_bracket      == input$income)
      if (input$channel != "All") d <- d %>% filter(acquisition_channel == input$channel)
      if (input$city    != "All") d <- d %>% filter(city                == input$city)
      if (isTRUE(input$sav)) d <- d %>% filter(savings_account    == 1)
      if (isTRUE(input$crd)) d <- d %>% filter(credit_card        == 1)
      if (isTRUE(input$ln))  d <- d %>% filter(personal_loan      == 1)
      if (isTRUE(input$inv)) d <- d %>% filter(investment_account == 1)
      if (isTRUE(input$ins)) d <- d %>% filter(insurance_product  == 1)
      d
    })
    
    # KPIs
    output$kpi_total <- renderText(scales::comma(nrow(df())))
    output$kpi_prod  <- renderText(round(mean(df()$active_products, na.rm = TRUE), 1))
    output$kpi_seg   <- renderText({
      df() %>% count(customer_segment) %>% slice_max(n, n = 1) %>%
        pull(customer_segment) %>% as.character()
    })
    output$kpi_age  <- renderText(round(mean(df()$age, na.rm = TRUE), 1))
    output$kpi_ch   <- renderText({
      df() %>% count(acquisition_channel) %>% slice_max(n, n = 1) %>%
        pull(acquisition_channel) %>% as.character()
    })
    output$kpi_freq <- renderText(
      round(mean(df()$transaction_frequency, na.rm = TRUE), 1))
    
    # Segment by Age stacked bar
    output$age_seg <- renderPlotly({
      d <- df() %>% filter(!is.na(age_group)) %>%
        count(age_group, customer_segment) %>%
        group_by(age_group) %>% mutate(pct = n / sum(n))
      p <- ggplot(d, aes(x = age_group, y = pct, fill = customer_segment,
                         text = paste0(customer_segment, ": ",
                                       scales::percent(pct, accuracy = 0.1)))) +
        geom_bar(stat = "identity", position = "stack",
                 colour = "#2C3E50", linewidth = 0.2) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(title = "Segment Distribution by Age Group",
             x = "Age Group", y = "Proportion", fill = "Segment") +
        theme_minimal(base_size = 11) + theme(legend.position = "bottom")
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", y = -0.25))
    })
    
    # Tx Frequency boxplot
    output$txfreq <- renderPlotly({
      d <- df() %>% filter(!is.na(age_group))
      p <- ggplot(d, aes(x = age_group, y = transaction_frequency,
                         fill = customer_segment)) +
        geom_boxplot(alpha = 0.7, position = position_dodge(0.8)) +
        scale_y_log10() +
        scale_fill_manual(values = seg_col) +
        labs(title = "Transaction Frequency by Age & Segment",
             x = "Age Group", y = "Tx Frequency (log)", fill = "Segment") +
        theme_minimal(base_size = 11) + theme(legend.position = "bottom")
      ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.25))
    })
    
    # Scatter: Tx Freq vs Value
    output$scatter <- renderPlotly({
      d <- df() %>% slice_sample(n = min(5000, nrow(df())))
      p <- ggplot(d, aes(x = transaction_frequency,
                         y = average_transaction_value / 1000,
                         color = customer_segment,
                         text = paste0("Segment: ", customer_segment,
                                       "<br>Freq: ",
                                       round(transaction_frequency, 1),
                                       "<br>Value (K): ",
                                       round(average_transaction_value / 1000, 1)))) +
        geom_point(alpha = 0.4, size = 1.2) +
        geom_smooth(method = "lm", se = FALSE, color = "#2C3E50", linewidth = 0.8) +
        scale_x_log10() +
        scale_color_manual(values = seg_col) +
        labs(title = "Transaction Frequency vs Value",
             x = "Tx Frequency (log)", y = "Avg Tx Value (K COP)",
             color = "Segment") +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", y = -0.15))
    })
    
    # Acquisition channel bar
    output$channel_plot <- renderPlotly({
      d <- df() %>% count(acquisition_channel) %>%
        mutate(acquisition_channel = fct_reorder(acquisition_channel, n))
      p <- ggplot(d, aes(x = acquisition_channel, y = n,
                         fill = acquisition_channel,
                         text = paste0(acquisition_channel, ": ",
                                       scales::comma(n)))) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() + scale_fill_brewer(palette = "Set2") +
        labs(title = "Acquisition Channel", x = NULL, y = "Count") +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text")
    })
    
    # Gender x Segment
    output$gender_seg <- renderPlotly({
      d <- df() %>% count(gender, customer_segment) %>%
        group_by(gender) %>% mutate(pct = n / sum(n))
      p <- ggplot(d, aes(x = gender, y = pct, fill = customer_segment,
                         text = paste0(customer_segment, ": ",
                                       scales::percent(pct, accuracy = 0.1)))) +
        geom_bar(stat = "identity", position = "stack",
                 colour = "#2C3E50", linewidth = 0.2) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(title = "Segment by Gender",
             x = NULL, y = "Proportion", fill = "Segment") +
        theme_minimal(base_size = 11) + theme(legend.position = "bottom")
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", y = -0.25))
    })
    
    # Income x Segment
    output$income_seg <- renderPlotly({
      d <- df() %>% count(income_bracket, customer_segment) %>%
        group_by(income_bracket) %>% mutate(pct = n / sum(n))
      p <- ggplot(d, aes(x = income_bracket, y = pct, fill = customer_segment,
                         text = paste0(customer_segment, ": ",
                                       scales::percent(pct, accuracy = 0.1)))) +
        geom_bar(stat = "identity", position = "stack",
                 colour = "#2C3E50", linewidth = 0.2) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(title = "Segment by Income Bracket",
             x = NULL, y = "Proportion", fill = "Segment") +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              legend.position = "bottom")
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", y = -0.3))
    })
    
    # Product adoption grouped bar
    output$product <- renderPlotly({
      d <- df() %>% group_by(customer_segment) %>%
        summarise(savings    = mean(savings_account),
                  credit     = mean(credit_card),
                  investment = mean(investment_account),
                  loan       = mean(personal_loan),
                  insurance  = mean(insurance_product)) %>%
        pivot_longer(-customer_segment,
                     names_to = "product", values_to = "adoption_rate")
      p <- ggplot(d, aes(x = product, y = adoption_rate,
                         fill = customer_segment,
                         text = paste0(customer_segment, " — ", product, ": ",
                                       scales::percent(adoption_rate, accuracy = 0.1)))) +
        geom_bar(stat = "identity", position = "dodge",
                 colour = "#2C3E50", linewidth = 0.2, alpha = 0.9) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(title = "Product Adoption Rate by Segment",
             x = "Product", y = "Adoption Rate", fill = "Segment") +
        theme_minimal(base_size = 11) + theme(legend.position = "bottom")
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", y = -0.15))
    })
    
    # Avg active products bar
    output$active_prod <- renderPlotly({
      d <- df() %>% group_by(customer_segment) %>%
        summarise(avg = mean(active_products, na.rm = TRUE)) %>%
        mutate(customer_segment = fct_reorder(customer_segment, avg))
      p <- ggplot(d, aes(x = customer_segment, y = avg,
                         fill = customer_segment,
                         text = paste0(customer_segment, ": ", round(avg, 2)))) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        scale_fill_manual(values = seg_col) + coord_flip() +
        labs(title = "Avg Active Products by Segment",
             x = NULL, y = "Avg Products") +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text")
    })
    
    # City heatmap (always full data)
    output$heatmap <- renderPlotly({
      d <- data %>% group_by(city) %>%
        summarise(
          Tx_Frequency      = mean(transaction_frequency,     na.rm = TRUE),
          Daily_Tx          = mean(avg_daily_transactions,    na.rm = TRUE),
          Tx_Value          = mean(average_transaction_value, na.rm = TRUE),
          App_Logins        = mean(app_logins_frequency,      na.rm = TRUE),
          Feature_Diversity = mean(feature_usage_diversity,   na.rm = TRUE),
          Credit_Util       = mean(credit_utilization_ratio,  na.rm = TRUE),
          Satisfaction      = mean(satisfaction_score,        na.rm = TRUE),
          NPS               = mean(nps_score,                 na.rm = TRUE),
          Tenure            = mean(customer_tenure,           na.rm = TRUE),
          Churn_Prob        = mean(churn_probability,         na.rm = TRUE)
        )
      mat <- d %>% column_to_rownames("city") %>% as.matrix()
      heatmaply(scale(mat), colors = viridis(100),
                Rowv = TRUE, Colv = TRUE, k_row = 4,
                xlab = "Behaviour Indicators", ylab = "Cities",
                showticklabels = c(TRUE, TRUE), dendrogram = "row",
                key.title = "Z-score", margins = c(60, 100, 40, 20))
    })
    
    # Explorer table
    output$table <- renderDT({
      df() %>%
        select(customer_id, age, age_group, gender, city,
               income_bracket, occupation, education_level,
               marital_status, acquisition_channel, customer_segment,
               active_products, savings_account, credit_card,
               personal_loan, investment_account, insurance_product) %>%
        datatable(
          filter = "top", extensions = "Buttons",
          options = list(pageLength = 10, scrollX = TRUE,
                         dom = "Bfrtip", buttons = c("copy","csv","excel"))
        )
    })
    
  })
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .kpi-card {
      background: #fff;
      border-radius: 8px;
      padding: 12px 14px;
      text-align: center;
      box-shadow: 0 2px 8px rgba(0,0,0,.07);
      border-top: 3px solid #4db8b8;
      margin-bottom: 10px;
    }
    .kpi-val { font-size: 1.5rem; font-weight: 700; color: #1a3a3a; }
    .kpi-lbl { font-size: .75rem; color: #7a9a9a; margin-top: 3px; }
    .control-card {
      background: #f4f8f8;
      border: 1px solid #cde0e0;
      border-radius: 8px;
      padding: 14px;
    }
    .output-card {
      background: #fff;
      border: 1px solid #e0eded;
      border-radius: 8px;
      padding: 14px;
    }
    .nav-tabs .nav-link.active { color: #1a6b6b; border-color: #4db8b8; }
    .nav-tabs .nav-link:hover  { color: #4db8b8; }
  "))),
  overview_ui("overview")
)

server <- function(input, output, session) {
  overview_server("overview", data = customer_data)
}

shinyApp(ui, server)