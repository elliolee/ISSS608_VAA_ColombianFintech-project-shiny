retention_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 2,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_customers"))),
            div(class = "kpi-lbl", "Filtered Customers"))
      ),
      column(
        width = 2,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_avg_clv"))),
            div(class = "kpi-lbl", "Avg CLV (M)"))
      ),
      column(
        width = 2,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_avg_churn"))),
            div(class = "kpi-lbl", "Avg Churn Risk"))
      ),
      column(
        width = 2,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_top_segment"))),
            div(class = "kpi-lbl", "Top CLV Segment"))
      ),
      column(
        width = 2,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_top_city"))),
            div(class = "kpi-lbl", "Top CLV City"))
      ),
      column(
        width = 2,
        div(class = "kpi-card",
            div(class = "kpi-val", textOutput(ns("kpi_rf_n"))),
            div(class = "kpi-lbl", "RF Eligible Obs."))
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
            inputId = ns("gender"),
            label = "Gender",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            inputId = ns("income"),
            label = "Income Bracket",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            inputId = ns("occupation"),
            label = "Occupation",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            inputId = ns("age_bucket"),
            label = "Age Group",
            choices = c("All"),
            selected = "All"
          ),
          
          selectInput(
            inputId = ns("segment"),
            label = "Customer Segment",
            choices = c("All"),
            selected = "All"
          ),
          
          br(),
          
          checkboxInput(
            inputId = ns("run_rf"),
            label = "Run Predictive Model (Random Forest)",
            value = FALSE
          ),
          
          hr(),
          
          p(
            style = "font-size: 12px; color: #6b7c7c;",
            "Use the filters to compare customer value, churn risk, and engagement patterns."
          ),
          p(
            style = "font-size: 12px; color: #6b7c7c;",
            "The predictive model is a diagnostic layer. For responsiveness, the model will train on a sampled subset when the filtered dataset is very large."
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
              "Value & Risk Overview",
              br(),
              fluidRow(
                column(6, plotOutput(ns("clv_plot"), height = "340px")),
                column(6, plotOutput(ns("churn_plot"), height = "340px"))
              ),
              br(),
              fluidRow(
                column(6, plotOutput(ns("engagement_plot"), height = "340px")),
                column(6, plotOutput(ns("map_plot"), height = "340px"))
              )
            ),
            
            tabPanel(
              "Predictive Modelling",
              br(),
              uiOutput(ns("rf_message")),
              plotOutput(ns("rf_plot"), height = "420px"),
              br(),
              tableOutput(ns("rf_metrics"))
            ),
            
            tabPanel(
              "Explorer Table",
              br(),
              DT::DTOutput(ns("retention_table"))
            )
          )
        )
      )
    )
  )
}

retention_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    req(data)
    
    fintech_clean <- reactive({
      d <- data
      
      if ("clv_millions" %in% names(d)) {
        d <- d %>% filter(!is.na(clv_millions))
      }
      if ("churn_probability" %in% names(d)) {
        d <- d %>% filter(!is.na(churn_probability))
      }
      if ("transaction_frequency" %in% names(d)) {
        d <- d %>% filter(!is.na(transaction_frequency))
      }
      
      d
    })
    
    observeEvent(fintech_clean(), {
      d <- fintech_clean()
      req(nrow(d) > 0)
      
      gender_choices <- d %>%
        pull(gender) %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        sort()
      
      income_choices <- d %>%
        pull(income_bracket) %>%
        as.character() %>%
        na.omit() %>%
        unique()
      
      occupation_choices <- d %>%
        pull(occupation) %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        sort()
      
      age_bucket_choices <- d %>%
        pull(age_bucket) %>%
        as.character() %>%
        na.omit() %>%
        unique()
      
      segment_choices <- d %>%
        pull(customer_segment) %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        sort()
      
      updateSelectInput(
        session, "gender",
        choices = c("All", gender_choices),
        selected = "All"
      )
      
      updateSelectInput(
        session, "income",
        choices = c("All", income_choices),
        selected = "All"
      )
      
      updateSelectInput(
        session, "occupation",
        choices = c("All", occupation_choices),
        selected = "All"
      )
      
      updateSelectInput(
        session, "age_bucket",
        choices = c("All", age_bucket_choices),
        selected = "All"
      )
      
      updateSelectInput(
        session, "segment",
        choices = c("All", segment_choices),
        selected = "All"
      )
    }, ignoreInit = FALSE)
    
    filtered_data <- reactive({
      df <- fintech_clean()
      
      if (input$gender != "All") {
        df <- df %>% filter(as.character(gender) == input$gender)
      }
      if (input$income != "All") {
        df <- df %>% filter(as.character(income_bracket) == input$income)
      }
      if (input$occupation != "All") {
        df <- df %>% filter(as.character(occupation) == input$occupation)
      }
      if (input$age_bucket != "All") {
        df <- df %>% filter(as.character(age_bucket) == input$age_bucket)
      }
      if (input$segment != "All") {
        df <- df %>% filter(as.character(customer_segment) == input$segment)
      }
      
      df
    })
    
    build_rf_data <- reactive({
      rf_data <- filtered_data() %>%
        select(
          clv_millions,
          transaction_frequency,
          active_products_app,
          average_transaction_value,
          satisfaction_score,
          nps_score,
          customer_segment,
          age_bucket,
          income_bracket,
          gender,
          occupation
        ) %>%
        na.omit() %>%
        rename(active_products = active_products_app)
      
      if (nrow(rf_data) > 5000) {
        set.seed(123)
        rf_data <- dplyr::slice_sample(rf_data, n = 5000)
      }
      
      rf_data
    })
    
    output$kpi_customers <- renderText({
      comma(nrow(filtered_data()))
    })
    
    output$kpi_avg_clv <- renderText({
      round(mean(filtered_data()$clv_millions, na.rm = TRUE), 2)
    })
    
    output$kpi_avg_churn <- renderText({
      percent(mean(filtered_data()$churn_probability, na.rm = TRUE), accuracy = 0.1)
    })
    
    output$kpi_top_segment <- renderText({
      d <- filtered_data() %>%
        group_by(customer_segment) %>%
        summarise(avg_clv = mean(clv_millions, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg_clv))
      
      if (nrow(d) == 0) return("NA")
      as.character(d$customer_segment[1])
    })
    
    output$kpi_top_city <- renderText({
      d <- filtered_data() %>%
        group_by(city) %>%
        summarise(avg_clv = mean(clv_millions, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg_clv))
      
      if (nrow(d) == 0) return("NA")
      as.character(d$city[1])
    })
    
    output$kpi_rf_n <- renderText({
      comma(nrow(build_rf_data()))
    })
    
    output$rf_message <- renderUI({
      if (!isTRUE(input$run_rf)) {
        div(
          style = "padding: 20px; color: #607575;",
          "Enable 'Run Predictive Model (Random Forest)' from the left panel to generate the prediction diagnostic plot."
        )
      }
    })
    
    output$clv_plot <- renderPlot({
      df <- filtered_data() %>%
        filter(!is.na(clv_millions), clv_millions > 0)
      
      validate(need(nrow(df) > 0, "No positive CLV data available for the selected filters."))
      
      ggplot(
        df,
        aes(x = customer_segment, y = clv_millions, fill = customer_segment)
      ) +
        geom_boxplot(alpha = 0.9, outlier.alpha = 0.25) +
        scale_fill_brewer(palette = "Blues", guide = "none") +
        scale_y_log10(labels = comma) +
        labs(
          title = "Customer Lifetime Value Distribution",
          x = "Customer Segment",
          y = "CLV (Millions, log scale)"
        ) +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold"))
    })
    
    output$churn_plot <- renderPlot({
      churn_by_segment <- filtered_data() %>%
        filter(!is.na(churn_probability)) %>%
        group_by(customer_segment) %>%
        summarise(
          avg_churn = mean(churn_probability, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(avg_churn))
      
      validate(need(nrow(churn_by_segment) > 0, "No churn data available."))
      
      ymin <- floor(min(churn_by_segment$avg_churn, na.rm = TRUE) * 100) / 100
      ymax <- ceiling(max(churn_by_segment$avg_churn, na.rm = TRUE) * 100) / 100
      if (ymax <= ymin) ymax <- ymin + 0.01
      
      ggplot(
        churn_by_segment,
        aes(x = reorder(customer_segment, avg_churn), y = avg_churn, fill = avg_churn)
      ) +
        geom_col(width = 0.7) +
        geom_text(
          aes(label = label_percent(accuracy = 0.1)(avg_churn)),
          vjust = -0.4,
          size = 4,
          color = "#08306b",
          fontface = "bold"
        ) +
        scale_fill_gradient(
          low = "#c6dbef",
          high = "#08306b",
          guide = "none"
        ) +
        scale_y_continuous(
          labels = label_percent(),
          expand = expansion(mult = c(0, 0.08))
        ) +
        coord_cartesian(ylim = c(ymin, ymax)) +
        labs(
          title = "Average Churn Risk by Segment",
          x = "Customer Segment",
          y = "Average Churn Probability"
        ) +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold"))
    })
    
    output$engagement_plot <- renderPlot({
      df <- filtered_data()
      validate(need(nrow(df) > 1, "Not enough data for engagement analysis."))
      
      x_limit <- quantile(df$transaction_frequency, 0.95, na.rm = TRUE)
      y_limit <- quantile(df$clv_millions, 0.95, na.rm = TRUE)
      
      ggplot(
        df,
        aes(
          x = transaction_frequency,
          y = clv_millions,
          color = customer_segment
        )
      ) +
        geom_point(alpha = 0.4) +
        geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = FALSE,
          linewidth = 0.8
        ) +
        scale_color_brewer(palette = "Blues") +
        coord_cartesian(
          xlim = c(0, x_limit),
          ylim = c(0, y_limit)
        ) +
        labs(
          title = "Engagement vs Customer Value",
          x = "Transaction Frequency",
          y = "CLV (Millions)",
          color = "Customer Segment"
        ) +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold"))
    })
    
    output$map_plot <- renderPlot({
      df <- filtered_data()
      
      validate(
        need(
          all(c("longitude", "latitude", "city", "department") %in% names(df)),
          "Map requires longitude, latitude, city, and department fields."
        )
      )
      
      colombia_map <- map_data("world", "Colombia")
      
      clv_location <- df %>%
        filter(
          !is.na(longitude), !is.na(latitude),
          longitude >= -80, longitude <= -67,
          latitude >= -5, latitude <= 13
        ) %>%
        group_by(city, department, latitude, longitude) %>%
        summarise(
          avg_clv = mean(clv_millions, na.rm = TRUE),
          customers = n(),
          .groups = "drop"
        )
      
      validate(need(nrow(clv_location) > 0, "No geographic data available for selected filters."))
      
      top_cities <- clv_location %>%
        arrange(desc(avg_clv)) %>%
        slice_head(n = 3)
      
      ggplot() +
        geom_polygon(
          data = colombia_map,
          aes(x = long, y = lat, group = group),
          fill = "#f7fbff",
          color = "gray80"
        ) +
        geom_point(
          data = clv_location,
          aes(
            x = longitude,
            y = latitude,
            color = avg_clv,
            size = customers
          ),
          alpha = 0.75
        ) +
        geom_text(
          data = top_cities,
          aes(
            x = longitude,
            y = latitude,
            label = city
          ),
          size = 3,
          nudge_y = 0.35
        ) +
        scale_color_gradient(
          low = "#9ecae1",
          high = "#08306b"
        ) +
        labs(
          title = "Geographic Customer Value",
          color = "Average CLV",
          size = "Customers"
        ) +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold"))
    })
    
    output$rf_plot <- renderPlot({
      req(input$run_rf)
      
      rf_data <- build_rf_data()
      
      validate(
        need(
          nrow(rf_data) >= 20,
          "Not enough complete observations to run the Random Forest model for the selected filters."
        )
      )
      
      set.seed(123)
      
      train_index <- sample(seq_len(nrow(rf_data)), size = floor(0.7 * nrow(rf_data)))
      train_data <- rf_data[train_index, , drop = FALSE]
      test_data  <- rf_data[-train_index, , drop = FALSE]
      
      validate(
        need(nrow(test_data) >= 5, "Not enough test observations after filtering.")
      )
      
      rf_model <- randomForest(
        clv_millions ~ .,
        data = train_data,
        ntree = 100
      )
      
      test_data$predicted_clv <- predict(rf_model, newdata = test_data)
      
      ggplot(
        test_data,
        aes(x = predicted_clv, y = clv_millions)
      ) +
        geom_point(alpha = 0.4, color = "#3182bd") +
        geom_abline(
          slope = 1,
          intercept = 0,
          linetype = "dashed",
          color = "#08306b",
          linewidth = 0.8
        ) +
        labs(
          title = "Random Forest: Predicted vs Actual CLV",
          x = "Predicted CLV",
          y = "Actual CLV"
        ) +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold"))
    })
    
    output$rf_metrics <- renderTable({
      req(input$run_rf)
      
      rf_data <- build_rf_data()
      
      validate(
        need(
          nrow(rf_data) >= 20,
          "Not enough complete observations to compute model metrics."
        )
      )
      
      set.seed(123)
      
      train_index <- sample(seq_len(nrow(rf_data)), size = floor(0.7 * nrow(rf_data)))
      train_data <- rf_data[train_index, , drop = FALSE]
      test_data  <- rf_data[-train_index, , drop = FALSE]
      
      validate(
        need(nrow(test_data) >= 5, "Not enough test observations after filtering.")
      )
      
      rf_model <- randomForest(
        clv_millions ~ .,
        data = train_data,
        ntree = 100
      )
      
      pred <- predict(rf_model, newdata = test_data)
      
      rmse <- sqrt(mean((test_data$clv_millions - pred)^2))
      mae  <- mean(abs(test_data$clv_millions - pred))
      r2   <- cor(test_data$clv_millions, pred)^2
      
      tibble(
        Metric = c("Training Sample Used", "Test Observations", "RMSE", "MAE", "R-squared"),
        Value = c(
          nrow(rf_data),
          nrow(test_data),
          round(rmse, 3),
          round(mae, 3),
          round(r2, 3)
        )
      )
    })
    
    output$retention_table <- renderDT({
      filtered_data() %>%
        select(
          customer_id,
          city,
          department,
          age,
          age_bucket,
          gender,
          income_bracket,
          occupation,
          customer_segment,
          transaction_frequency,
          average_transaction_value,
          active_products_app,
          churn_probability,
          customer_lifetime_value,
          clv_millions
        ) %>%
        rename(active_products = active_products_app) %>%
        DT::datatable(
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