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
.
├── app.R                  # Main app entry point
├── global.R               # Global setup (data + libraries)
├── modules/               # Modular UI + server logic
├── scripts/
│   └── 01_data_cleaning.R # Data preprocessing pipeline
├── data_raw/              # Raw dataset (not tracked in git ideally)
├── data_clean/            # Cleaned dataset used by app
├── www/                   # CSS / assets
├── rsconnect/             # Deployment config (auto-generated)
├── .gitignore

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

Users access the app directly from the website：https://elliolee.github.io/ISSS608_VAA_ColombianFintech-project-website/
