# 📊 Colombian Fintech Customer Analytics – Shiny App

This repository contains the **interactive Shiny dashboard** for the ISSS608 Visual Analytics group project.

🔗 Live App:  
https://ligen.shinyapps.io/isss608_vaa_colombianfintech-project-shiny/

---

## 📌 Project Objective

The Shiny application enables interactive exploration of customer analytics in a Colombian fintech dataset.

It supports three key analytical modules:

1. Customer Overview & Segmentation  
2. Customer Engagement & Experience  
3. Retention Risk & Customer Value  

---

## 🧱 Project Structure

```text
.
├── app.R
├── global.R
├── modules/
│   ├── module_overview.R
│   ├── module_engagement.R
│   └── module_retention.R
├── scripts/
│   └── 01_data_cleaning.R
├── data_raw/
│   └── customer_data.csv
├── data_clean/
│   └── customer_data_cleaned.csv
├── www/
│   └── .gitkeep
├── rsconnect/
│   └── shinyapps.io/
├── .gitignore
└── ISSS608_VAA_ColombianFintech-project-shiny.Rproj

---

## 🔄 Data Workflow

The project follows a **clean pipeline design**:
data_raw → scripts/01_data_cleaning.R → data_clean → Shiny App

Shiny app always reads from data_clean.

## ▶️ Run App Locally
shiny::runApp()

## 🚀 Deployment
Deployed via shinyapps.io using rsconnect.
rsconnect::deployApp()

## 🔗 Relationship with Website
	•	Website = presentation + navigation
	•	Shiny App = interactive analytics engine

Users access the app directly from the website：
https://elliolee.github.io/ISSS608_VAA_ColombianFintech-project-website/
