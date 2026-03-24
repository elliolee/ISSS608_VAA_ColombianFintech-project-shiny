library(tidyverse)
library(lubridate)
library(forcats)

# =========================
# Paths
# =========================
raw_path   <- "data_raw/customer_data.csv"
clean_path <- "data_clean/customer_data_cleaned.csv"

# =========================
# Load raw data
# =========================
df <- read_csv(raw_path, show_col_types = FALSE)

# =========================
# 1. Remove exact duplicate field
# first_tx duplicates first_transaction_date
# =========================
if ("first_tx" %in% names(df) && "first_transaction_date" %in% names(df)) {
  same_first <- all(df$first_tx == df$first_transaction_date, na.rm = TRUE)
  if (same_first) {
    df <- df %>% select(-first_tx)
  }
}

# =========================
# 2. Convert date columns
# =========================
date_cols <- c("first_transaction_date", "last_tx", "last_transaction_date", "last_survey_date")

date_cols <- intersect(date_cols, names(df))

df <- df %>%
  mutate(across(all_of(date_cols), ~ mdy(.x)))

# =========================
# 3. Handle missing values
# =========================

# complaint_topics: likely no complaint if missing
if ("complaint_topics" %in% names(df)) {
  df <- df %>%
    mutate(complaint_topics = replace_na(complaint_topics, "None"))
}

# feature_requests: treat NA as no request
if ("feature_requests" %in% names(df)) {
  df <- df %>%
    mutate(feature_requests = replace_na(feature_requests, "None"))
}

# credit_utilization_ratio:
# keep numeric column, add missing flag, and impute only for non-credit-card users
if ("credit_utilization_ratio" %in% names(df) && "credit_card" %in% names(df)) {
  df <- df %>%
    mutate(
      credit_utilization_missing = if_else(is.na(credit_utilization_ratio), "Missing", "Observed"),
      credit_utilization_ratio = if_else(
        is.na(credit_utilization_ratio) & credit_card %in% c("No", "FALSE", "False", "0", 0),
        0,
        credit_utilization_ratio
      )
    )
}

# =========================
# 4. Standardise categorical levels
# =========================
categorical_cols <- c(
  "gender", "city", "department", "income_bracket", "occupation",
  "education_level", "marital_status", "acquisition_channel",
  "customer_segment", "savings_account", "credit_card", "personal_loan",
  "investment_account", "insurance_product", "bill_payment_user",
  "auto_savings_enabled", "feedback_sentiment", "feature_requests",
  "complaint_topics", "clv_segment", "preferred_transaction_type"
)

categorical_cols <- intersect(categorical_cols, names(df))

df <- df %>%
  mutate(across(all_of(categorical_cols), ~ as.character(.x))) %>%
  mutate(across(all_of(categorical_cols), ~ str_trim(.x))) %>%
  mutate(across(all_of(categorical_cols), ~ na_if(.x, ""))) %>%
  mutate(across(all_of(categorical_cols), ~ replace_na(.x, "Unknown"))) %>%
  mutate(across(all_of(categorical_cols), ~ as.factor(.x))) %>%
  mutate(across(all_of(categorical_cols), ~ fct_infreq(.x)))

# =========================
# 5. Numeric validation / type conversion
# =========================
numeric_cols <- c(
  "age", "latitude", "longitude", "household_size", "active_products",
  "app_logins_frequency", "feature_usage_diversity", "credit_utilization_ratio",
  "international_transactions", "failed_transactions", "tx_count", "avg_tx_value",
  "total_tx_volume", "base_satisfaction", "tx_satisfaction", "product_satisfaction",
  "satisfaction_score", "nps_score", "support_tickets_count", "resolved_tickets_ratio",
  "app_store_rating", "monthly_transaction_count", "average_transaction_value",
  "total_transaction_volume", "transaction_frequency", "weekend_transaction_ratio",
  "avg_daily_transactions", "customer_tenure", "churn_probability",
  "customer_lifetime_value"
)

numeric_cols <- intersect(numeric_cols, names(df))

df <- df %>%
  mutate(across(all_of(numeric_cols), as.numeric))

# =========================
# 6. Range checks / light cleaning
# =========================
# Keep rows, but clip or set unreasonable values to NA where necessary

if ("churn_probability" %in% names(df)) {
  df <- df %>%
    mutate(churn_probability = if_else(churn_probability < 0 | churn_probability > 1, NA_real_, churn_probability))
}

if ("resolved_tickets_ratio" %in% names(df)) {
  df <- df %>%
    mutate(resolved_tickets_ratio = if_else(resolved_tickets_ratio < 0 | resolved_tickets_ratio > 1, NA_real_, resolved_tickets_ratio))
}

if ("weekend_transaction_ratio" %in% names(df)) {
  df <- df %>%
    mutate(weekend_transaction_ratio = if_else(weekend_transaction_ratio < 0 | weekend_transaction_ratio > 1, NA_real_, weekend_transaction_ratio))
}

# =========================
# 7. Cross-field consistency checks
# =========================

# active_products vs binary product columns
product_cols <- c("savings_account", "credit_card", "personal_loan", "investment_account", "insurance_product")
product_cols <- intersect(product_cols, names(df))

if (length(product_cols) > 0 && "active_products" %in% names(df)) {
  df <- df %>%
    mutate(
      active_products_recalc = rowSums(across(all_of(product_cols), ~ .x %in% c("Yes", "TRUE", "True", "1", 1)), na.rm = TRUE),
      active_products_match = if_else(active_products == active_products_recalc, "Match", "Mismatch")
    )
}

# tenure consistency check
if (all(c("customer_tenure", "first_transaction_date") %in% names(df))) {
  df <- df %>%
    mutate(
      tenure_check_flag = if_else(is.na(first_transaction_date), "Missing first date", "Available")
    )
}

# =========================
# 8. Keep notes on overlapping variables
# These are NOT exact duplicates, so retain both for now:
# - last_tx vs last_transaction_date
# - avg_tx_value vs average_transaction_value
# - total_tx_volume vs total_transaction_volume
# =========================

# =========================
# Save cleaned data
# =========================
write_csv(df, clean_path)

message("Cleaned dataset saved to: ", clean_path)