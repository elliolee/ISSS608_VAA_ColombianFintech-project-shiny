library(shiny)
library(bslib)
library(tidyverse)

# shared theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#10b981",
  secondary = "#123c69",
  bg = "#f8fafc",
  fg = "#1f2937",
  base_font = font_google("Inter")
)

# shared cleaned dataset
customer_data <- NULL
if (file.exists("data_clean/customer_data_cleaned.csv")) {
  customer_data <- readr::read_csv("data_clean/customer_data_cleaned.csv", show_col_types = FALSE)
}