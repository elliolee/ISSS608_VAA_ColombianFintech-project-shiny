overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
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
          selectInput(ns("seg"), "Customer Segment",
                      choices = c("All", "inactive", "occasional", "regular", "power")),
          selectInput(ns("gender"), "Gender",
                      choices = c("All")),
          selectInput(ns("income"), "Income Bracket",
                      choices = c("All", "Very Low", "Low", "Medium", "High", "Very High")),
          selectInput(ns("channel"), "Acquisition Channel",
                      choices = c("All")),
          selectInput(ns("city"), "City",
                      choices = c("All")),
          hr(),
          h5("Product Owned"),
          checkboxInput(ns("sav"), "Savings Account", FALSE),
          checkboxInput(ns("crd"), "Credit Card", FALSE),
          checkboxInput(ns("ln"), "Personal Loan", FALSE),
          checkboxInput(ns("inv"), "Investment Account", FALSE),
          checkboxInput(ns("ins"), "Insurance Product", FALSE)
        )
      ),
      
      column(
        width = 9,
        div(
          class = "output-card",
          h4("Outputs"),
          tabsetPanel(
            tabPanel(
              "Segment by Age",
              br(),
              fluidRow(
                column(6, plotlyOutput(ns("age_seg"), height = "400px")),
                column(6, plotlyOutput(ns("txfreq"), height = "400px"))
              )
            ),
            tabPanel(
              "Behaviour Analysis",
              br(),
              fluidRow(
                column(8, plotlyOutput(ns("scatter"), height = "420px")),
                column(4, plotlyOutput(ns("channel_plot"), height = "420px"))
              ),
              br(),
              fluidRow(
                column(6, plotlyOutput(ns("gender_seg"), height = "360px")),
                column(6, plotlyOutput(ns("income_seg"), height = "360px"))
              )
            ),
            tabPanel(
              "Product Adoption",
              br(),
              fluidRow(
                column(8, plotlyOutput(ns("product"), height = "420px")),
                column(4, plotlyOutput(ns("active_prod"), height = "420px"))
              )
            ),
            tabPanel(
              "City Heatmap",
              br(),
              plotlyOutput(ns("heatmap"), height = "570px")
            ),
            tabPanel(
              "Explorer Table",
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
    
    req(data)
    
    observe({
      updateSelectInput(
        session, "gender",
        choices = c("All", sort(unique(as.character(data$gender))))
      )
      updateSelectInput(
        session, "channel",
        choices = c("All", sort(unique(as.character(data$acquisition_channel))))
      )
      updateSelectInput(
        session, "city",
        choices = c("All", sort(unique(as.character(data$city))))
      )
    })
    
    df <- reactive({
      d <- data
      
      if (input$seg != "All") {
        d <- d %>% filter(as.character(customer_segment) == input$seg)
      }
      if (input$gender != "All") {
        d <- d %>% filter(as.character(gender) == input$gender)
      }
      if (input$income != "All") {
        d <- d %>% filter(as.character(income_bracket) == input$income)
      }
      if (input$channel != "All") {
        d <- d %>% filter(as.character(acquisition_channel) == input$channel)
      }
      if (input$city != "All") {
        d <- d %>% filter(as.character(city) == input$city)
      }
      
      if (isTRUE(input$sav)) d <- d %>% filter(savings_account_num == 1)
      if (isTRUE(input$crd)) d <- d %>% filter(credit_card_num == 1)
      if (isTRUE(input$ln))  d <- d %>% filter(personal_loan_num == 1)
      if (isTRUE(input$inv)) d <- d %>% filter(investment_account_num == 1)
      if (isTRUE(input$ins)) d <- d %>% filter(insurance_product_num == 1)
      
      d
    })
    
    output$kpi_total <- renderText({
      comma(nrow(df()))
    })
    
    output$kpi_prod <- renderText({
      round(mean(df()$active_products_app, na.rm = TRUE), 1)
    })
    
    output$kpi_seg <- renderText({
      d <- df() %>%
        count(customer_segment) %>%
        slice_max(n, n = 1, with_ties = FALSE)
      
      if (nrow(d) == 0) return("NA")
      as.character(d$customer_segment[1])
    })
    
    output$kpi_age <- renderText({
      round(mean(df()$age, na.rm = TRUE), 1)
    })
    
    output$kpi_ch <- renderText({
      d <- df() %>%
        count(acquisition_channel) %>%
        slice_max(n, n = 1, with_ties = FALSE)
      
      if (nrow(d) == 0) return("NA")
      as.character(d$acquisition_channel[1])
    })
    
    output$kpi_freq <- renderText({
      round(mean(df()$transaction_frequency, na.rm = TRUE), 1)
    })
    
    output$age_seg <- renderPlotly({
      d <- df() %>%
        filter(!is.na(age_group)) %>%
        count(age_group, customer_segment) %>%
        group_by(age_group) %>%
        mutate(pct = n / sum(n))
      
      validate(need(nrow(d) > 0, "No data available for the selected filters."))
      
      p <- ggplot(
        d,
        aes(
          x = age_group, y = pct, fill = customer_segment,
          text = paste0(customer_segment, ": ", percent(pct, accuracy = 0.1))
        )
      ) +
        geom_bar(stat = "identity", position = "stack",
                 colour = "#2C3E50", linewidth = 0.2) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(
          title = "Segment Distribution by Age Group",
          x = "Age Group", y = "Proportion", fill = "Segment"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.38,
            font = list(size = 11)
          ),
          margin = list(b = 120)
        )
    })
    
    output$txfreq <- renderPlotly({
      d <- df() %>% filter(!is.na(age_group))
      
      validate(need(nrow(d) > 0, "No transaction frequency data available."))
      
      p <- ggplot(
        d,
        aes(x = age_group, y = transaction_frequency, fill = customer_segment)
      ) +
        geom_boxplot(alpha = 0.7, position = position_dodge(0.8)) +
        scale_y_log10() +
        scale_fill_manual(values = seg_col) +
        labs(
          title = "Transaction Frequency by Age & Segment",
          x = "Age Group", y = "Tx Frequency (log)", fill = "Segment"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      
      ggplotly(p) %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.38,
            font = list(size = 11)
          ),
          margin = list(b = 120)
        )
    })
    
    output$scatter <- renderPlotly({
      d <- df()
      
      validate(need(nrow(d) > 1, "Not enough data for scatter analysis."))
      
      d <- d %>% slice_sample(n = min(5000, nrow(d)))
      
      p <- ggplot(
        d,
        aes(
          x = transaction_frequency,
          y = average_transaction_value / 1000,
          color = customer_segment,
          text = paste0(
            "Segment: ", customer_segment,
            "<br>Freq: ", round(transaction_frequency, 1),
            "<br>Value (K): ", round(average_transaction_value / 1000, 1)
          )
        )
      ) +
        geom_point(alpha = 0.4, size = 1.2) +
        geom_smooth(method = "lm", se = FALSE, color = "#2C3E50", linewidth = 0.8) +
        scale_x_log10() +
        scale_color_manual(values = seg_col) +
        labs(
          title = "Transaction Frequency vs Value",
          x = "Tx Frequency (log)",
          y = "Avg Tx Value (K COP)",
          color = "Segment"
        ) +
        theme_minimal(base_size = 11)
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.30,
            font = list(size = 11)
          ),
          margin = list(b = 95)
        )
    })
    
    output$channel_plot <- renderPlotly({
      d <- df() %>%
        count(acquisition_channel) %>%
        mutate(acquisition_channel = fct_reorder(acquisition_channel, n))
      
      validate(need(nrow(d) > 0, "No channel data available."))
      
      p <- ggplot(
        d,
        aes(
          x = acquisition_channel, y = n, fill = acquisition_channel,
          text = paste0(acquisition_channel, ": ", comma(n))
        )
      ) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Acquisition Channel", x = NULL, y = "Count") +
        theme_minimal(base_size = 11)
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(b = 70))
    })
    
    output$gender_seg <- renderPlotly({
      d <- df() %>%
        count(gender, customer_segment) %>%
        group_by(gender) %>%
        mutate(pct = n / sum(n))
      
      validate(need(nrow(d) > 0, "No gender-segment data available."))
      
      p <- ggplot(
        d,
        aes(
          x = gender, y = pct, fill = customer_segment,
          text = paste0(customer_segment, ": ", percent(pct, accuracy = 0.1))
        )
      ) +
        geom_bar(stat = "identity", position = "stack",
                 colour = "#2C3E50", linewidth = 0.2) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(
          title = "Segment by Gender",
          x = NULL, y = "Proportion", fill = "Segment"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.38,
            font = list(size = 11)
          ),
          margin = list(b = 120)
        )
    })
    
    output$income_seg <- renderPlotly({
      d <- df() %>%
        count(income_bracket, customer_segment) %>%
        group_by(income_bracket) %>%
        mutate(pct = n / sum(n))
      
      validate(need(nrow(d) > 0, "No income-segment data available."))
      
      p <- ggplot(
        d,
        aes(
          x = income_bracket, y = pct, fill = customer_segment,
          text = paste0(customer_segment, ": ", percent(pct, accuracy = 0.1))
        )
      ) +
        geom_bar(stat = "identity", position = "stack",
                 colour = "#2C3E50", linewidth = 0.2) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(
          title = "Segment by Income Bracket",
          x = NULL, y = "Proportion", fill = "Segment"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "bottom"
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.42,
            font = list(size = 11)
          ),
          margin = list(b = 130)
        )
    })
    
    output$product <- renderPlotly({
      d <- df() %>%
        group_by(customer_segment) %>%
        summarise(
          savings    = mean(savings_account_num, na.rm = TRUE),
          credit     = mean(credit_card_num, na.rm = TRUE),
          investment = mean(investment_account_num, na.rm = TRUE),
          loan       = mean(personal_loan_num, na.rm = TRUE),
          insurance  = mean(insurance_product_num, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(-customer_segment,
                     names_to = "product", values_to = "adoption_rate")
      
      validate(need(nrow(d) > 0, "No product adoption data available."))
      
      p <- ggplot(
        d,
        aes(
          x = product, y = adoption_rate, fill = customer_segment,
          text = paste0(
            customer_segment, " — ", product, ": ",
            percent(adoption_rate, accuracy = 0.1)
          )
        )
      ) +
        geom_bar(stat = "identity", position = "dodge",
                 colour = "#2C3E50", linewidth = 0.2, alpha = 0.9) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = seg_col) +
        labs(
          title = "Product Adoption Rate by Segment",
          x = "Product", y = "Adoption Rate", fill = "Segment"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.32,
            font = list(size = 11)
          ),
          margin = list(b = 105)
        )
    })
    
    output$active_prod <- renderPlotly({
      d <- df() %>%
        group_by(customer_segment) %>%
        summarise(avg = mean(active_products_app, na.rm = TRUE), .groups = "drop") %>%
        mutate(customer_segment = fct_reorder(customer_segment, avg))
      
      validate(need(nrow(d) > 0, "No active product data available."))
      
      p <- ggplot(
        d,
        aes(
          x = customer_segment, y = avg, fill = customer_segment,
          text = paste0(customer_segment, ": ", round(avg, 2))
        )
      ) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        scale_fill_manual(values = seg_col) +
        coord_flip() +
        labs(
          title = "Avg Active Products by Segment",
          x = NULL, y = "Avg Products"
        ) +
        theme_minimal(base_size = 11)
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(b = 70))
    })
    
    output$heatmap <- renderPlotly({
      validate(
        need(
          all(c(
            "city", "transaction_frequency", "avg_daily_transactions",
            "average_transaction_value", "app_logins_frequency",
            "feature_usage_diversity", "credit_utilization_ratio",
            "satisfaction_score", "nps_score", "customer_tenure",
            "churn_probability"
          ) %in% names(data)),
          "Required fields for city heatmap are missing."
        )
      )
      
      d <- data %>%
        group_by(city) %>%
        summarise(
          Tx_Frequency      = mean(transaction_frequency, na.rm = TRUE),
          Daily_Tx          = mean(avg_daily_transactions, na.rm = TRUE),
          Tx_Value          = mean(average_transaction_value, na.rm = TRUE),
          App_Logins        = mean(app_logins_frequency, na.rm = TRUE),
          Feature_Diversity = mean(feature_usage_diversity, na.rm = TRUE),
          Credit_Util       = mean(credit_utilization_ratio, na.rm = TRUE),
          Satisfaction      = mean(satisfaction_score, na.rm = TRUE),
          NPS               = mean(nps_score, na.rm = TRUE),
          Tenure            = mean(customer_tenure, na.rm = TRUE),
          Churn_Prob        = mean(churn_probability, na.rm = TRUE),
          .groups = "drop"
        )
      
      validate(need(nrow(d) > 1, "Not enough city-level data for heatmap."))
      
      mat <- d %>%
        column_to_rownames("city") %>%
        as.matrix()
      
      heatmaply(
        scale(mat),
        colors = viridis(100),
        Rowv = TRUE,
        Colv = TRUE,
        k_row = 4,
        xlab = "Behaviour Indicators",
        ylab = "Cities",
        showticklabels = c(TRUE, TRUE),
        dendrogram = "row",
        key.title = "Z-score",
        margins = c(60, 100, 40, 20)
      )
    })
    
    output$table <- renderDT({
      df() %>%
        select(
          customer_id, age, age_group, gender, city,
          income_bracket, occupation, education_level,
          marital_status, acquisition_channel, customer_segment,
          active_products_app, savings_account_num, credit_card_num,
          personal_loan_num, investment_account_num, insurance_product_num
        ) %>%
        rename(
          active_products = active_products_app,
          savings_account = savings_account_num,
          credit_card = credit_card_num,
          personal_loan = personal_loan_num,
          investment_account = investment_account_num,
          insurance_product = insurance_product_num
        ) %>%
        datatable(
          filter = "top",
          extensions = "Buttons",
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel")
          )
        )
    })
    
  })
}