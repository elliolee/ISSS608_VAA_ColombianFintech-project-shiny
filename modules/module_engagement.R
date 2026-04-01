engagement_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_sat"))),
            div(class = "kpi-lbl", "Average Satisfaction"))
      ),
      column(
        width = 3,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_nps"))),
            div(class = "kpi-lbl", "Average NPS"))
      ),
      column(
        width = 3,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_tickets"))),
            div(class = "kpi-lbl", "Average Support Tickets"))
      ),
      column(
        width = 3,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_feature"))),
            div(class = "kpi-lbl", "Average Feature Usage Diversity"))
      )
    ),
    
    br(),
    
    fluidRow(
      column(
        width = 3,
        div(
          class = "control-card",
          h4("Inputs"),
          
          selectInput(
            ns("segment"),
            "Customer Segment",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            ns("channel"),
            "Acquisition Channel",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            ns("sentiment"),
            "Feedback Sentiment",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            ns("complaint"),
            "Complaint Topic",
            choices = c("All"),
            selected = "All"
          ),
          
          checkboxGroupInput(
            ns("products"),
            "Product Ownership",
            choices = c(
              "Savings Account" = "savings_account_num",
              "Credit Card" = "credit_card_num",
              "Personal Loan" = "personal_loan_num",
              "Investment Account" = "investment_account_num",
              "Insurance Product" = "insurance_product_num"
            ),
            selected = character(0)
          ),
          
          selectInput(
            ns("ticket_bucket"),
            "Support Ticket Bucket",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            ns("usage_bucket"),
            "Feature Usage Bucket",
            choices = c("All"),
            selected = "All"
          ),
          
          hr(),
          
          p(
            style = "font-size: 12px; color: #6b7c7c;",
            "This module diagnoses customer engagement, service experience, and operational friction."
          ),
          p(
            style = "font-size: 12px; color: #6b7c7c;",
            "Feature usage breadth is treated as a stronger engagement signal than simple login frequency."
          )
        )
      ),
      
      column(
        width = 9,
        div(
          class = "output-card",
          h4("Outputs"),
          
          tabsetPanel(
            tabPanel(
              "Engagement & Experience",
              br(),
              fluidRow(
                column(6, plotOutput(ns("feature_plot"), height = "350px")),
                column(6, plotOutput(ns("login_plot"), height = "350px"))
              ),
              br(),
              fluidRow(
                column(6, plotOutput(ns("nps_sat_plot"), height = "350px")),
                column(6, plotOutput(ns("nps_usage_plot"), height = "350px"))
              )
            ),
            
            tabPanel(
              "Service Friction",
              br(),
              fluidRow(
                column(6, plotOutput(ns("complaint_plot"), height = "360px")),
                column(6, plotOutput(ns("nps_ticket_plot"), height = "360px"))
              )
            ),
            
            tabPanel(
              "Summary Tables",
              br(),
              h5("Product-level Engagement Summary"),
              DT::DTOutput(ns("engagement_table")),
              br(),
              h5("Product-level Login Summary"),
              DT::DTOutput(ns("login_table"))
            )
          )
        )
      )
    )
  )
}

engagement_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    req(data)
    
    engagement_base <- reactive({
      d <- data %>%
        mutate(
          complaint_topics = if_else(
            is.na(as.character(complaint_topics)) | as.character(complaint_topics) == "",
            "None",
            as.character(complaint_topics)
          ),
          
          feedback_sentiment = as.character(feedback_sentiment),
          
          support_ticket_bucket = case_when(
            is.na(support_tickets_count) ~ NA_character_,
            support_tickets_count == 0 ~ "0",
            support_tickets_count == 1 ~ "1",
            support_tickets_count == 2 ~ "2",
            support_tickets_count >= 3 ~ "3+",
            TRUE ~ NA_character_
          ),
          
          feature_usage_bucket = case_when(
            is.na(feature_usage_diversity) ~ NA_character_,
            feature_usage_diversity <= 1 ~ "0-1",
            feature_usage_diversity == 2 ~ "2",
            feature_usage_diversity == 3 ~ "3",
            feature_usage_diversity >= 4 ~ "4+",
            TRUE ~ NA_character_
          ),
          
          support_ticket_bucket = factor(
            support_ticket_bucket,
            levels = c("0", "1", "2", "3+")
          ),
          
          feature_usage_bucket = factor(
            feature_usage_bucket,
            levels = c("0-1", "2", "3", "4+")
          ),
          
          satisfaction_score = factor(
            as.character(satisfaction_score),
            levels = sort(unique(as.numeric(as.character(satisfaction_score))))
          )
        )
      
      d
    })
    
    observeEvent(engagement_base(), {
      d <- engagement_base()
      req(nrow(d) > 0)
      
      updateSelectInput(
        session, "segment",
        choices = c("All", sort(unique(as.character(d$customer_segment)))),
        selected = "All"
      )
      
      updateSelectInput(
        session, "channel",
        choices = c("All", sort(unique(as.character(d$acquisition_channel)))),
        selected = "All"
      )
      
      updateSelectInput(
        session, "sentiment",
        choices = c("All", sort(unique(as.character(d$feedback_sentiment)))),
        selected = "All"
      )
      
      updateSelectInput(
        session, "complaint",
        choices = c("All", sort(unique(as.character(d$complaint_topics)))),
        selected = "All"
      )
      
      updateSelectInput(
        session, "ticket_bucket",
        choices = c("All", levels(d$support_ticket_bucket)),
        selected = "All"
      )
      
      updateSelectInput(
        session, "usage_bucket",
        choices = c("All", levels(d$feature_usage_bucket)),
        selected = "All"
      )
    }, ignoreInit = FALSE)
    
    filtered_data <- reactive({
      d <- engagement_base()
      
      if (input$segment != "All") {
        d <- d %>% filter(as.character(customer_segment) == input$segment)
      }
      
      if (input$channel != "All") {
        d <- d %>% filter(as.character(acquisition_channel) == input$channel)
      }
      
      if (input$sentiment != "All") {
        d <- d %>% filter(as.character(feedback_sentiment) == input$sentiment)
      }
      
      if (input$complaint != "All") {
        d <- d %>% filter(as.character(complaint_topics) == input$complaint)
      }
      
      if (input$ticket_bucket != "All") {
        d <- d %>% filter(as.character(support_ticket_bucket) == input$ticket_bucket)
      }
      
      if (input$usage_bucket != "All") {
        d <- d %>% filter(as.character(feature_usage_bucket) == input$usage_bucket)
      }
      
      # Product ownership filter rule:
      # keep rows where at least one selected product is owned
      if (length(input$products) > 0) {
        keep_idx <- rep(FALSE, nrow(d))
        for (col_name in input$products) {
          if (col_name %in% names(d)) {
            keep_idx <- keep_idx | (d[[col_name]] == 1)
          }
        }
        d <- d[keep_idx, , drop = FALSE]
      }
      
      d
    })
    
    product_engagement_summary <- reactive({
      d <- filtered_data() %>%
        select(
          feature_usage_diversity,
          savings_account_num,
          credit_card_num,
          personal_loan_num,
          investment_account_num,
          insurance_product_num
        ) %>%
        pivot_longer(
          cols = c(
            savings_account_num,
            credit_card_num,
            personal_loan_num,
            investment_account_num,
            insurance_product_num
          ),
          names_to = "product_type",
          values_to = "owned"
        ) %>%
        mutate(
          product_type = recode(
            product_type,
            savings_account_num = "Savings Account",
            credit_card_num = "Credit Card",
            personal_loan_num = "Personal Loan",
            investment_account_num = "Investment Account",
            insurance_product_num = "Insurance Product"
          ),
          ownership_status = if_else(owned == 1, "Own", "Not Own")
        ) %>%
        group_by(product_type, ownership_status) %>%
        summarise(
          avg_feature_diversity = mean(feature_usage_diversity, na.rm = TRUE),
          .groups = "drop"
        )
      
      d
    })
    
    product_login_summary <- reactive({
      d <- filtered_data() %>%
        select(
          app_logins_frequency,
          savings_account_num,
          credit_card_num,
          personal_loan_num,
          investment_account_num,
          insurance_product_num
        ) %>%
        pivot_longer(
          cols = c(
            savings_account_num,
            credit_card_num,
            personal_loan_num,
            investment_account_num,
            insurance_product_num
          ),
          names_to = "product_type",
          values_to = "owned"
        ) %>%
        mutate(
          product_type = recode(
            product_type,
            savings_account_num = "Savings Account",
            credit_card_num = "Credit Card",
            personal_loan_num = "Personal Loan",
            investment_account_num = "Investment Account",
            insurance_product_num = "Insurance Product"
          ),
          ownership_status = if_else(owned == 1, "Own", "Not Own")
        ) %>%
        group_by(product_type, ownership_status) %>%
        summarise(
          avg_logins = mean(app_logins_frequency, na.rm = TRUE),
          .groups = "drop"
        )
      
      d
    })
    
    complaint_summary_actual <- reactive({
      d <- filtered_data() %>%
        filter(as.character(complaint_topics) != "None") %>%
        count(complaint_topics, sort = TRUE)
      
      d
    })
    
    output$kpi_sat <- renderText({
      round(mean(filtered_data()$satisfaction_score %>% as.character() %>% as.numeric(), na.rm = TRUE), 2)
    })
    
    output$kpi_nps <- renderText({
      round(mean(filtered_data()$nps_score, na.rm = TRUE), 1)
    })
    
    output$kpi_tickets <- renderText({
      round(mean(filtered_data()$support_tickets_count, na.rm = TRUE), 2)
    })
    
    output$kpi_feature <- renderText({
      round(mean(filtered_data()$feature_usage_diversity, na.rm = TRUE), 2)
    })
    
    output$feature_plot <- renderPlot({
      d <- product_engagement_summary()
      validate(need(nrow(d) > 0, "No product engagement summary available."))
      
      ggplot(
        d,
        aes(
          x = product_type,
          y = avg_feature_diversity,
          fill = ownership_status
        )
      ) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Not Own" = "#A7DDE3", "Own" = "#2D8F9B")) +
        labs(
          title = "Average Feature Usage Diversity by Product Ownership",
          subtitle = "Product owners show broader platform usage than non-owners.",
          x = "Product Type",
          y = "Average Feature Usage Diversity",
          fill = "Ownership Status"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1),
          plot.subtitle = element_text(size = 10)
        )
    })
    
    output$login_plot <- renderPlot({
      d <- product_login_summary()
      validate(need(nrow(d) > 0, "No product login summary available."))
      
      ggplot(
        d,
        aes(
          x = product_type,
          y = avg_logins,
          fill = ownership_status
        )
      ) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Not Own" = "#A7DDE3", "Own" = "#2D8F9B")) +
        labs(
          title = "Average App Login Frequency by Product Ownership",
          subtitle = "Login frequency is a secondary diagnostic indicator with weaker separation.",
          x = "Product Type",
          y = "Average App Login Frequency",
          fill = "Ownership Status"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1),
          plot.subtitle = element_text(size = 10)
        )
    })
    
    output$nps_sat_plot <- renderPlot({
      d <- filtered_data() %>%
        filter(!is.na(satisfaction_score), !is.na(nps_score))
      
      validate(need(nrow(d) > 0, "No satisfaction-NPS data available."))
      
      sat_levels <- sort(unique(as.numeric(as.character(d$satisfaction_score))))
      sat_levels_chr <- as.character(sat_levels)
      
      palette_vals <- c(
        "1" = "#E6F3F4",
        "2" = "#DCEFF1",
        "3" = "#B7DDE2",
        "4" = "#89C8CF",
        "5" = "#5DB2BA",
        "6" = "#2D8F9B"
      )
      palette_used <- palette_vals[names(palette_vals) %in% sat_levels_chr]
      
      ggplot(
        d,
        aes(
          x = factor(as.character(satisfaction_score), levels = sat_levels_chr),
          y = nps_score,
          fill = factor(as.character(satisfaction_score), levels = sat_levels_chr)
        )
      ) +
        geom_boxplot(alpha = 0.85, outlier.alpha = 0.2) +
        scale_fill_manual(values = palette_used, drop = FALSE) +
        labs(
          title = "NPS Score by Satisfaction Score",
          subtitle = "Satisfaction is strongly associated with customer advocacy.",
          x = "Satisfaction Score",
          y = "NPS Score"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "none",
          plot.subtitle = element_text(size = 10)
        )
    })
    
    output$nps_usage_plot <- renderPlot({
      d <- filtered_data() %>%
        filter(!is.na(feature_usage_bucket), !is.na(nps_score))
      
      validate(need(nrow(d) > 0, "No feature usage-NPS data available."))
      
      ggplot(
        d,
        aes(
          x = feature_usage_bucket,
          y = nps_score,
          fill = feature_usage_bucket
        )
      ) +
        geom_boxplot(alpha = 0.85, outlier.alpha = 0.2) +
        scale_fill_manual(values = c(
          "0-1" = "#DCEFF1",
          "2" = "#A7DDE3",
          "3" = "#6FC7CF",
          "4+" = "#2D8F9B"
        )) +
        labs(
          title = "NPS Score by Feature Usage Diversity Bucket",
          subtitle = "The relationship is modest compared with satisfaction.",
          x = "Feature Usage Diversity Bucket",
          y = "NPS Score"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "none",
          plot.subtitle = element_text(size = 10)
        )
    })
    
    output$complaint_plot <- renderPlot({
      d <- complaint_summary_actual()
      validate(need(nrow(d) > 0, "No complaint records available after filtering."))
      
      complaint_palette <- c(
        "Customer service" = "#2D8F9B",
        "Fees" = "#49A8B3",
        "Transaction issues" = "#63BEC7",
        "Account security" = "#85D0D5",
        "App performance" = "#A7DDE3"
      )
      
      ggplot(
        d,
        aes(
          x = fct_reorder(complaint_topics, n),
          y = n,
          fill = complaint_topics
        )
      ) +
        geom_col(show.legend = FALSE) +
        geom_text(
          aes(label = n),
          hjust = -0.15,
          size = 3.5
        ) +
        coord_flip() +
        scale_fill_manual(values = complaint_palette) +
        expand_limits(y = max(d$n) + max(d$n) * 0.12) +
        labs(
          title = "Complaint Topics Distribution (Excluding 'None')",
          subtitle = "Complaint categories are broadly distributed rather than dominated by one issue.",
          x = "Complaint Topic",
          y = "Number of Customers"
        ) +
        theme_minimal(base_size = 11) +
        theme(plot.subtitle = element_text(size = 10))
    })
    
    output$nps_ticket_plot <- renderPlot({
      d <- filtered_data() %>%
        filter(!is.na(support_ticket_bucket), !is.na(nps_score))
      
      validate(need(nrow(d) > 0, "No support ticket-NPS data available."))
      
      ggplot(
        d,
        aes(
          x = support_ticket_bucket,
          y = nps_score,
          fill = support_ticket_bucket
        )
      ) +
        geom_boxplot(alpha = 0.85, outlier.alpha = 0.2) +
        scale_fill_manual(values = c(
          "0" = "#DCEFF1",
          "1" = "#A7DDE3",
          "2" = "#6FC7CF",
          "3+" = "#2D8F9B"
        )) +
        labs(
          title = "NPS Score by Support Ticket Bucket",
          subtitle = "Support burden shows only a modest relationship with advocacy.",
          x = "Support Ticket Bucket",
          y = "NPS Score"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "none",
          plot.subtitle = element_text(size = 10)
        )
    })
    
    output$engagement_table <- DT::renderDT({
      product_engagement_summary() %>%
        mutate(avg_feature_diversity = round(avg_feature_diversity, 2)) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
        )
    })
    
    output$login_table <- DT::renderDT({
      product_login_summary() %>%
        mutate(avg_logins = round(avg_logins, 2)) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
        )
    })
    
  })
}