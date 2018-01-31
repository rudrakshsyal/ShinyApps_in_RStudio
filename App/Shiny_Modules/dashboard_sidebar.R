## Dashboard Sidebar

sidebarUI <- function(id){
  ns <- NS(id)
  
  dashboardSidebar(
  width = 250,
  disable = F,
  
  sidebarMenu(
    id = "tabs",
    menuItem("1. Users & Transactions", icon = icon("search-plus"), startExpanded = T, 
             menuSubItem(" a. Traffic", tabName = "Traffic", icon = icon("users")),
             menuSubItem(" b. Transactions", tabName = "Transactions", icon = icon("bitcoin")),
             menuSubItem(" c. Liquidity", tabName = "Liquidity", icon = icon("bar-chart")),
             menuSubItem(" d. GMV / VPE", tabName = "GMV_VPE", icon = icon("usd")),
             menuSubItem(" e. Conversion", tabName = "Conversion", icon = icon("hourglass-2"))),
    menuItem("2. CPT Overview", icon = icon("inr"), startExpanded = T,
             menuSubItem(" a. Weekly Tracker", tabName = "cpt_weekly", icon = icon("bar-chart")),
             menuSubItem(" b. Monetisation", tabName = "cpt_monetisation", icon = icon("inr")),
             menuSubItem(" c. FQS", tabName = "cpt_fqs", icon = icon("stethoscope")),
             menuSubItem(" d. Acquisition & Churn", tabName = "cpt_churn", icon = icon("minus-circle"))),
    menuItem("3. Reach Dynamics", tabName = "Reach", badgeLabel = "Upcoming", badgeColor = "red", icon = icon("inr")),
    menuItem("4. Content", tabName = "Content", badgeLabel = "Upcoming", badgeColor = "red", icon = icon("user-o")),
    menuItem("5. Feedback", tabName = "Feedback", badgeLabel = "Upcoming", badgeColor = "red", icon = icon("user-o")),
    hr()
  ),
  
  selectInput(inputId = ns("country"), label = "Country - Filter", 
              choices = c("India","Singapore","Brazil","Indonesia","Philippines","Rest of the World"), 
              selected = 'India', multiple = T),
  hr()
  )}


sidebar <- function(input, output, session){
  country <- reactive({input$country})
  return(country)
}