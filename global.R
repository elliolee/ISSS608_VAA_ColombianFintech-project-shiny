library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(heatmaply)
library(viridis)
library(shinyWidgets)
library(maps)
library(randomForest)

# =========================
# Shared theme
# =========================
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#10b981",
  secondary = "#123c69",
  bg = "#f8fafc",
  fg = "#1f2937",
  base_font = font_google("Inter")
)

# =========================
# Shared palette
# =========================
seg_col <- c(
  "inactive"   = "#7f9baa",
  "occasional" = "#4db8b8",
  "regular"    = "#a8d5d5",
  "power"      = "#1a6b6b"
)

# =========================
# Helper functions
# =========================
to_binary_flag <- function(x) {
  x_chr <- toupper(trimws(as.character(x)))
  if_else(x_chr %in% c("YES", "TRUE", "1", "Y"), 1L, 0L)
}

safe_factor <- function(x) {
  factor(as.character(x))
}

# =========================
# Shared cleaned dataset
# =========================
data_path <- "data_clean/customer_data_cleaned.csv"

if (!file.exists(data_path)) {
  stop("Cleaned dataset not found at data_clean/customer_data_cleaned.csv")
}

customer_data <- readr::read_csv(
  data_path,
  show_col_types = FALSE
)

customer_data <- customer_data %>%
  mutate(
    savings_account_num = if ("savings_account" %in% names(.)) to_binary_flag(savings_account) else 0L,
    credit_card_num = if ("credit_card" %in% names(.)) to_binary_flag(credit_card) else 0L,
    personal_loan_num = if ("personal_loan" %in% names(.)) to_binary_flag(personal_loan) else 0L,
    investment_account_num = if ("investment_account" %in% names(.)) to_binary_flag(investment_account) else 0L,
    insurance_product_num = if ("insurance_product" %in% names(.)) to_binary_flag(insurance_product) else 0L,
    
    active_products_app =
      savings_account_num +
      credit_card_num +
      personal_loan_num +
      investment_account_num +
      insurance_product_num
  )

if ("age" %in% names(customer_data)) {
  customer_data <- customer_data %>%
    mutate(
      age_group = cut(
        age,
        breaks = c(18, 25, 35, 45, 55, 70),
        labels = c("18-25", "26-35", "36-45", "46-55", "55+"),
        include.lowest = TRUE
      ),
      age_bucket = cut(
        age,
        breaks = c(18, 25, 35, 45, 55, 65, Inf),
        labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"),
        include.lowest = TRUE
      )
    )
}

if ("customer_lifetime_value" %in% names(customer_data)) {
  customer_data <- customer_data %>%
    mutate(
      clv_millions = customer_lifetime_value / 1000000
    )
}

if ("income_bracket" %in% names(customer_data)) {
  customer_data <- customer_data %>%
    mutate(
      income_bracket = factor(
        as.character(income_bracket),
        levels = c("Very Low", "Low", "Medium", "High", "Very High")
      )
    )
}

for (col in c("gender", "city", "department", "occupation",
              "education_level", "marital_status",
              "acquisition_channel", "customer_segment")) {
  if (col %in% names(customer_data)) {
    customer_data[[col]] <- safe_factor(customer_data[[col]])
  }
}