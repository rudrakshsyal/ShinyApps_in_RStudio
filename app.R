setwd("~/Documents/MIS/dashboard")

## 1. Load Libraries ----
library(htmltools)
library(lubridate)
library(dplyr)
library(reshape)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(zoo)
library(reshape2)
library(Hmisc)
library(DT)
library(extrafont)
# font_import()
# loadfonts(device = "win")
library(ggthemes)
library(htmlwidgets)
library(gtable)
library(grid)
library(plotly)
library(sunburstR)
# devtools::install_github("carlganz/rintrojs")
library(rintrojs)
library(shinyWidgets)
library(data.table)
# install.packages("shinyjs")
library(shinyjs)
# devtools::install_github("yang-tang/shinyjqui")
library(shinyjqui)

## 2. Reading the Process Folder files ----

update_date <- Sys.Date() - 1
Total_Transactions_Country <- read.csv("../Process/Total_Transactions_Country.csv", stringsAsFactors = F)[,-1]
Total_Transactions_City <- read.csv("../Process/Total_Transactions_City.csv", stringsAsFactors = F)[,-1]
Total_Transactions_Country$month_names <- as.Date(Total_Transactions_Country$month_names, format = '%Y-%m-%d')
Total_Transactions_City$month_names <- as.Date(Total_Transactions_City$month_names, format = '%Y-%m-%d')

Device_Country <- read.csv("../Process/device_users.csv", stringsAsFactors = F)[,-1]
Device_Country$month <- as.Date(Device_Country$month, format = '%Y-%m-%d')

Country_Users <- read.csv("../Process/Country_Users.csv", stringsAsFactors = F)[,-1]
Country_Users$month_names <- as.Date(Country_Users$month_names, format = '%Y-%m-%d')

Country_Transactions_Conversion_percentage <- read.csv("../Process/Country_Transactions_Conversion_percentage.csv", stringsAsFactors = F)[,-1]
Country_Transactions_Conversion_percentage$month_names <- as.Date(Country_Transactions_Conversion_percentage$month_names, format = '%Y-%m-%d')

Channel_Country <- read.csv("../Process/channel_country.csv", stringsAsFactors = F)[,-1]
Channel_Country$month_names <- as.Date(Channel_Country$month_names, format = '%Y-%m-%d')

Users_Current_Month <- read.csv("../Process/Users_Current_Month.csv", stringsAsFactors = F)[,-1]
Users_Previous_Month <- read.csv("../Process/Users_Previous_Month.csv", stringsAsFactors = F)[,-1]

Transactions_Current_Month <- read.csv("../Process/Transactions_Current_Month.csv", stringsAsFactors = F)[,-1]
Transactions_Current_Month_City <- read.csv("../Process/Transactions_Current_Month_City.csv", stringsAsFactors = F)[,-1]
Transactions_Previous_Month <- read.csv("../Process/Transactions_Previous_Month.csv", stringsAsFactors = F)[,-1]
Transactions_Previous_Month_City <- read.csv("../Process/Transactions_Previous_Month_City.csv", stringsAsFactors = F)[,-1]

City_Transactions_Conversion_percentage <- read.csv("../Process/City_Transactions_Conversion_percentage.csv", stringsAsFactors = F)[,-1]

City_Transactions_Conversion_percentage$month_names <- as.Date(City_Transactions_Conversion_percentage$month_names, format = '%Y-%m-%d')

Spec_Transactions_Conversion_percentage <- read.csv("../Process/Spec_Transactions_Conversion_percentage.csv", stringsAsFactors = F)[,-1]
Spec_Transactions_Conversion_percentage$month <- as.Date(Spec_Transactions_Conversion_percentage$month, format = '%Y-%m-%d')
City_Spec_Transactions_Conversion_percentage <- read.csv("../Process/City_Spec_Transactions_Conversion_percentage.csv", stringsAsFactors = F)[,-1]
City_Spec_Transactions_Conversion_percentage$month <- as.Date(City_Spec_Transactions_Conversion_percentage$month, format = '%Y-%m-%d')

Conversion_Spec_Current_Month <- read.csv("../Process/conversion_spec_current_month.csv", stringsAsFactors = F)[,-1]

Cancellations_Country <- read.csv("../Process/Cancellations_Country.csv", stringsAsFactors = F)[,-1]
Cancellations_Country$month_names <- as.Date(Cancellations_Country$month_names, format = '%Y-%m-%d')
Cancellations_City <- read.csv("../Process/Cancellations_City.csv", stringsAsFactors = F)[,-1]
Cancellations_City$month_names <- as.Date(Cancellations_City$month_names, format = '%Y-%m-%d')

Transactions_Month_City_Data_Table <- read.csv("../process/transactions_month_city_data_table.csv", stringsAsFactors = F)[,-1]

Book_Practo_Source <- read.csv("../process/book_practo_source.csv", stringsAsFactors = F)[,-1]
Book_Practo_Source$month_names <- as.Date(Book_Practo_Source$month_names, format = '%Y-%m-%d')

archive_dod <- read.csv("../process/archive_dod.csv", stringsAsFactors = F)[,-1]
transactions_run_rate <- read.csv("../process/transactions_dod.csv", stringsAsFactors = F)[,-1]

liquidity_current_month <- read.csv("../process/liquidity_current_month.csv", stringsAsFactors = F)[,-1]
liquidity_previous_month <- read.csv("../process/liquidity_previous_month.csv", stringsAsFactors = F)[,-1]
vpe_current_month <- read.csv("../process/vpe_current_month.csv", stringsAsFactors = F)[,-1]
vpe_previous_month <- read.csv("../process/vpe_previous_month.csv", stringsAsFactors = F)[,-1]

liquidity_mom <- read.csv("../process/liquidity_mom.csv", stringsAsFactors = F)[,-1]
liquidity_mom$month <- as.Date(liquidity_mom$month, format = '%Y-%m-%d')

temp_2 <- read.csv("../process/temp_2.csv", stringsAsFactors = F)[,-1]

gmv_vpe <- read.csv("../process/gmv_vpe.csv", stringsAsFactors = F)[,-1]
gmv_vpe$month <- as.Date(gmv_vpe$month, format = '%Y-%m-%d')

segregated_gmv <- read.csv("../process/segregated_gmv.csv", stringsAsFactors = F)[,-1]
segregated_gmv$month <- as.Date(segregated_gmv$month, format = '%Y-%m-%d')

final_vpe_gmv <- read.csv("../process/final_vpe_gmv.csv", stringsAsFactors = F)[,-1]
final_vpe_gmv$month <- as.Date(final_vpe_gmv$month, format = '%Y-%m-%d')

segregated_vpe <- read.csv("../process/segregated_vpe.csv", stringsAsFactors = F)[,-1]
segregated_vpe$month <- as.Date(segregated_vpe$month, format = '%Y-%m-%d')

dch_archive <- read.csv("../process/dch_archive.csv", stringsAsFactors = F)[,-1]
dch_archive$month <- as.Date(dch_archive$month, format = '%Y-%m-%d')

dch_archive_city <- read.csv("../process/DCH_Archive_city.csv", stringsAsFactors = F)[,-1]
dch_archive_city$month <- as.Date(dch_archive_city$month, format = '%Y-%m-%d')

data_plot_A <- read.csv("../process/tranx_type.csv", stringsAsFactors = F)[,-1]
data_plot_B <- read.csv("../process/pie_absVSreach.csv", stringsAsFactors = F)[,-1]
data_plot_C <- read.csv("../process/absVSreach.csv", stringsAsFactors = F)[,-1]
data_plot_D <- read.csv("../process/live_cust_sub.csv", stringsAsFactors = F)[,-1]
data_plot_E <- read.csv("../process/newVSexisting.csv", stringsAsFactors = F)[,-1]
data_plot_F <- read.csv("../process/avg_duration.csv", stringsAsFactors = F)[,-1]
data_plot_G <- read.csv("../process/ctr.csv", stringsAsFactors = F)[,-1]
data_plot_H <- read.csv("../process/liq_reach.csv", stringsAsFactors = F)[,-1]

data_plot_A$month <- as.Date(data_plot_A$month, format = '%Y-%m-%d')
data_plot_C$month <- as.Date(data_plot_C$month, format = '%Y-%m-%d')
data_plot_D$month <- as.Date(data_plot_D$month, format = '%Y-%m-%d')
data_plot_E$month <- as.Date(data_plot_E$month, format = '%Y-%m-%d')
data_plot_F$month <- as.Date(data_plot_F$month, format = '%Y-%m-%d')
data_plot_G$month <- as.Date(data_plot_G$month, format = '%Y-%m-%d')

users_profile_listing <- read.csv("../process/users_profile_listing.csv", stringsAsFactors = F)[,-1]
users_profile_listing$date <- as.Date(users_profile_listing$date, format = '%Y-%m-%d')

users_profile_listing_tier <- read.csv("../process/users_profile_listing_tier.csv", stringsAsFactors = F)[,-1]
users_profile_listing_tier$date <- as.Date(users_profile_listing_tier$date, format = '%Y-%m-%d')

new_conversion_percentage <- read.csv("../process/new_conversion_percentage.csv", stringsAsFactors = F)[,-1]
new_conversion_percentage$month_names <- as.Date(new_conversion_percentage$month_names, format = '%Y-%m-%d')

unique_patient_tranx <- read.csv("../process/unique_patient_tranx.csv", stringsAsFactors = F)[,-1]
unique_patient_tranx$month <- as.Date(unique_patient_tranx$month, format = '%Y-%m-%d')

unique_patient_tranx_city <- read.csv("../process/unique_patient_tranx_city.csv", stringsAsFactors = F)[,-1]
unique_patient_tranx_city$month <- as.Date(unique_patient_tranx_city$month, format = '%Y-%m-%d')

## 3. ui.R ----

ui <- dashboardPage(
  title = "Marketplace Dashboard",
  skin = "black",
  
  
  dashboardHeader(
    
    dropdownMenu(
      type = "notifications",
      
      notificationItem(
        text = (paste0("Last Updated on - ", update_date)),
        icon = icon("welcome"),
        status = "warning"
      )),
    title = " Marketplace Dashboard ",
    titleWidth = 250,
    disable = F
  ),
  
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
               menuSubItem(" e. Conversion", tabName = "Conversion", icon = icon("line-chart"))),
      menuItem("2. Reach Dynamics", tabName = "RD", badgeLabel = "NEW", badgeColor = "red", icon = icon("inr")),
      menuItem("3. Content", tabName = "C", badgeLabel = "Upcoming", badgeColor = "yellow", icon = icon("book")),
      menuItem("4. Feedback", tabName = "F", badgeLabel = "Upcoming", badgeColor = "green", icon = icon("user-o")),
      hr()
    ),
    
    # dropdownButton(
    #   # tags$h3("Country - Filter"),
    #   selectInput(inputId = "country", label = "Country - Filter", choices = unique(Country_Users$country), selected = 'India', multiple = T),
    #   circle = F, status = "danger", icon = icon("gear"), width = "200px",
    #   tooltip = tooltipOptions(title = "Click to see the Country - Filter!")
    # ),
    
    selectInput(inputId = "country", label = "Country - Filter", choices = unique(Country_Users$country), selected = 'India', multiple = T),
    
    hr()
    
    # helpText(paste0("Last updated on: ", update_date))
  ),
  
  dashboardBody(
    
    tags$head(includeScript("google-analytics.js")),
    # tags$head(tags$link(rel="shortcut icon", href="./favicon.ico")),
    # jqui_draggabled(fileInput('ComboChart1a', 'File')),
    # includeCSS("cerulean.css"),
    
    tabItems(
      
      ## 1. Users and Transactions ----
      
      ############################ a. Traffic ----
      tabItem(
        tabName = "Traffic",
        
        fluidRow(
          valueBoxOutput("users_c", width = 3),
          valueBoxOutput("transactions_c", width = 3),
          valueBoxOutput("vpe_c", width = 3),
          valueBoxOutput("liquidity_c", width = 3)
        ),
        
        fluidRow(
          valueBoxOutput("users_p", width = 3),
          valueBoxOutput("transactions_p", width = 3),
          valueBoxOutput("vpe_p", width = 3),
          valueBoxOutput("liquidity_p", width = 3)
        ),
        
        # shinyjs::useShinyjs(),
        # actionButton("showSidebar", "Show the Sidebar Panel", icon = icon("arrow-circle-right")),
        # actionButton("hideSidebar", "Hide the Sidebar Panel", icon = icon("arrow-circle-left")),
        
        fluidRow(
          
          box(width = 12, title = "I. Traffic", status = "primary", solidHeader = F, collapsible = T, collapsed = F, 
              
              tags$i("TOTAL USERS - Total Practo traffic, including DCH, Consult, Diagnostics, Healthfeed, AMP and Mobile App"),
              br(),
              tags$i("MARKETPLACE USERS - Includes DCH Traffic, AMP and Mobile App"),
              hr(),
              
              tabBox(width = 28, id = "tabset0",
                     
                     tabPanel("a. by Channel", plotlyOutput('trf_1a_combo')),
                     
                     tabPanel("b. by Source", plotlyOutput("trf_1b_line")),
                     
                     tabPanel("c. by Segment-Device",
                              
                              column(12, checkboxGroupButtons(
                                inputId = "segment1", label = "Please select the Segment: ", 
                                choices = unique(users_profile_listing$segment), 
                                selected = "Clinic",
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, checkboxGroupButtons(
                                inputId = "device", label = "Please select the Device: ", 
                                choices = unique(users_profile_listing$deviceCategory), 
                                selected = "mobile",
                                justified = T, 
                                size = "sm",
                                status = "danger",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotlyOutput("trf_1c_line"))
                              
                     ),
                     
                     tabPanel("d. by Segment-Tier",
                              
                              column(12, checkboxGroupButtons(
                                inputId = "segment2", label = "Please select the Segment: ", 
                                choices = unique(users_profile_listing$segment), 
                                selected = "Clinic",
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, checkboxGroupButtons(
                                inputId = "tier", label = "Please select the Tier: ", 
                                choices = unique(users_profile_listing_tier$tier), 
                                selected = "Tier - 1",
                                justified = T, 
                                size = "sm",
                                status = "success",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotlyOutput("trf_1d_line"))
                     ),
                     tabPanel("e. by Speciality",
                              
                              column(12, 
                                     radioButtons("tt", 
                                                  "Please select the Metric: ",
                                                  choiceNames = c('Traffic (Current Month)', 'Transactions (Current Month)'),
                                                  choiceValues = c('users', 'transactions'),
                                                  selected = "transactions", 
                                                  inline = T)),
                              
                              column(10, sunburstOutput("trf_1e_pie", width = "100%", height = "400px"))
                     ),
                     tabPanel("f. by Tier",
                              
                              column(12, checkboxGroupButtons(
                                inputId = "tier_users", label = "Please select the Tier: ", 
                                choices = unique(dch_archive_city$tier), 
                                selected = "Tier-1",
                                justified = T, 
                                size = "xs",
                                status = "success",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotlyOutput("trf_1f_column")))
              )
          ), 
          
          box(width = 12, title = "II. Daily Run Rate", status = "primary", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset3",
                     
                     # tabPanel("Transactions Run Rate", plotlyOutput("LineChart1a")),
                     tabPanel("a. Traffic", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "channel", label = "Please select the Channel: ", 
                                choices = unique(Channel_Country$channels), 
                                selected = "Direct",
                                justified = F, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(10, plotlyOutput("trf_2a_line"))
                     )
              )
          )
        )
      ),
      ############################ b. Liquidity ----
      tabItem(
        tabName = "Liquidity",
        
        fluidRow(
          
          box(width = 12, title = "I. Liquidity - India", status = "success", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset4",
                     
                     tabPanel("a. Month-on-Month trend", plotOutput("liq_1a_column")),
                     tabPanel("b. Area Under the Curve", plotlyOutput("liq_1b_line"))
              )
          )
        )
      ),
      ############################ c. GMV / VPE ----
      tabItem(
        tabName = "GMV_VPE",
        
        fluidRow(
          
          box(width = 12, title =  "I. Marketplace GMV & VPE", status = "warning", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset2",
                     
                     tabPanel("a. Overall", plotOutput("gmv_1a_combo")),
                     tabPanel("b. by Business Unit", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "bu", label = "Please select the Business Unit: ", 
                                choices = c("Clinic" = 'clinic', "Mid - Market" = 'mm', "Strategic - Accounts" = 'sa'),
                                selected = "sa",
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotOutput("gmv_1b_combo"))
                     )
              )
          )
        )
      ),
      ############################ d. Transactions ----
      tabItem(
        tabName = "Transactions",
        
        fluidRow(
          
          box(width = 12, title =  "I. Transactions", status = "danger", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset1",
                     
                     tabPanel("a. by Mode", plotlyOutput("tra_1a_combo")),
                     tabPanel("b. Monetizable & Unique Tranxs.", plotlyOutput("tra_1b_column")),
                     tabPanel("c. by Source", plotlyOutput("tra_1c_line")),
                     tabPanel("d. by Segment", plotlyOutput("tra_1d_column")),
                     tabPanel("e. by Speciality",
                              column(12, 
                                     radioButtons("tt", 
                                                  "Please select the Metric: ",
                                                  choiceNames = c('Traffic (Current Month)', 'Transactions (Current Month)'),
                                                  choiceValues = c('users', 'transactions'),
                                                  selected = "transactions", 
                                                  inline = T)),
                              
                              column(10, sunburstOutput("tra_1e_pie", width = "100%", height = "400px"))
                     ),
                     tabPanel("f. by City", 
                              column(12, 
                                     DT::dataTableOutput("tra_1f_table", width = 500))),
                     
                     tabPanel("g. Unique Tranxs. by Tier",
                              column(12, checkboxGroupButtons(
                                inputId = "tier_unique", label = "Please select the Tier: ", 
                                choices = unique(unique_patient_tranx_city$tier),
                                selected = "Tier-1",
                                justified = T, 
                                size = "xs",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              column(12, plotlyOutput("tra_1g_column"))
                              )
              )
          ),
          
          box(width = 12, title = "II. Transactions", status = "danger", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset3",
                     
                     tabPanel("a. Daily Run Rate", plotlyOutput("tra_2a_line"))
              )
          ),
          
          box(width = 12, title =  "III. ABS Cancellation Rate", status = "danger", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset1",
                     
                     tabPanel("a. by Country", plotlyOutput("tra_3a_line")),
                     tabPanel("b. by City",
                              column(12, checkboxGroupButtons(
                                inputId = "city_cancellation", label = "Please select the City: ", 
                                choices = unique(Transactions_Month_City_Data_Table$city), 
                                selected = "Pune",
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotlyOutput("tra_3b_line"))
                     )
              )
          )
        )
      ),
      ############################ e. Conversion ----
      tabItem(
        tabName = "Conversion",
        
        fluidRow(
          
          box(width = 12, title =  "I. Conversion %", status = "info", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset2",
                     
                     tabPanel("a. Confirmed Transactions / Unique DCH Users", plotOutput("cnv_1a_column")),
                     tabPanel("b. Unique Patients / Unique DCH Users", plotOutput("cnv_1b_column")),
                     tabPanel("c. by City", 
                              column(12, checkboxGroupButtons(
                                inputId = "city_conversion", label = "Please select the City: ", 
                                choices = unique(Transactions_Month_City_Data_Table$city), 
                                selected = "Bengaluru",
                                justified = TRUE, 
                                status = "primary",
                                size = "sm",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              column(10, plotlyOutput("cnv_2a_line"))
                     )
              )
          ),
          
          box(width = 12, title =  "II. Conversion %", status = "info", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset3",
                     
                     tabPanel("a. Top Specialities", plotlyOutput("cnv_3a_bar")),
                     
                     tabPanel("b. Trend for Top Specialities", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "spec_conv", label = "Please select the Speciality: ", 
                                choices = unique(City_Spec_Transactions_Conversion_percentage$speciality),
                                selected = "dentist",
                                justified = F, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(10, plotlyOutput("cnv_3b_line"))
                     ),
                     
                     tabPanel("c. by City-Speciality", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "city", label = "Please select the City: ", 
                                choices = unique(City_Spec_Transactions_Conversion_percentage$city), 
                                selected = "Delhi_NCR",
                                justified = T, 
                                individual = F,
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(10, plotlyOutput("cnv_3c_line"))
                     )
              )
          )
          
        )
      ),
      
      ## 2. Reach Dynamics ----
      
      tabItem(
        tabName = "RD",
        
        # fluidRow(
        #   valueBoxOutput("live_accounts_c", width = 3),
        #   valueBoxOutput("live_subs_c", width = 3),
        #   valueBoxOutput("reach_card_ctr_c", width = 3),
        #   valueBoxOutput("percent_appts_reach_card_c", width = 3)
        # ),
        
        # fluidRow(
        #   valueBoxOutput("live_accounts_p", width = 3),
        #   valueBoxOutput("live_subs_p", width = 3),
        #   valueBoxOutput("reach_card_ctr_p", width = 3),
        #   valueBoxOutput("percent_appts_reach_card_p", width = 3)
        # ),
        
        fluidRow(
          
          box(width = 12, title =  " % Transactions Monetized", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset111",
                     
                     # tabPanel("Month-on-Month absolute figures", 
                     #          
                     #          column(12, checkboxGroupButtons(
                     #            inputId = "reach_city_1", label = "Please select the City: ", 
                     #            choices = unique(data_plot_A$city_new),
                     #            selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                     #            justified = T, 
                     #            size = "sm",
                     #            status = "primary",
                     #            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                     #          )
                     #          ),
                     #          
                     #          column(12, plotlyOutput("reach_barchart1"))
                     # ),
                     
                     tabPanel("Month-on-Month percentage figures", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "reach_city_2", label = "Please select the City: ", 
                                choices = unique(data_plot_A$city_new),
                                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotlyOutput("reach_barchart2"))
                     )
              )
          ),
          
          box(width = 12, title =  "City-wise ABS v/s Reach Card Appointments (Current Month) ", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
              
              tabBox(width = 28, id = "tabset222",
                     
                     tabPanel("City-wise ABS Appointments", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "sunburst_city_1", label = "Please select the City: ", 
                                choices = unique(data_plot_B$city_new),
                                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12,sunburstOutput("reach_sunburst1"))
                     ),
                     
                     tabPanel("City-wise Reach Card Appointments", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "sunburst_city_2", label = "Please select the City: ", 
                                choices = unique(data_plot_B$city_new),
                                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12,sunburstOutput("reach_sunburst2"))
                     )
              )
          ),
          
          box(width = 12, title =  "City-wise ABS v/s Reach Card Appointments (Month-on-Month Trend) ", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
              
              column(12, checkboxGroupButtons(
                inputId = "reach_city_3", label = "Please select the City: ", 
                choices = unique(data_plot_C$city_new),
                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                justified = T, 
                size = "sm",
                status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              )
              ),
              
              column(12, plotlyOutput("reach_facet_linechart1"))
              
          ),
          
          box(width = 12, title =  "City-wise Live Customers & Subscriptions (Month-on-Month Trend) ", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
              
              column(12, checkboxGroupButtons(
                inputId = "reach_city_4", label = "Please select the City: ", 
                choices = unique(data_plot_D$city_new),
                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                justified = T, 
                size = "sm",
                status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              )
              ),
              
              column(12, plotlyOutput("reach_linechart1"))
              
          ),
          
          box(width = 12, title =  "City-wise New & Repeat Customers (Month-on-Month trend)", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset333",
                     
                     tabPanel("Month-on-Month absolute figures", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "reach_city_5", label = "Please select the City: ", 
                                choices = unique(data_plot_E$city_new),
                                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotlyOutput("reach_barchart3"))
                     ),
                     
                     tabPanel("Month-on-Month percentage figures", 
                              
                              column(12, checkboxGroupButtons(
                                inputId = "reach_city_6", label = "Please select the City: ", 
                                choices = unique(data_plot_E$city_new),
                                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                                justified = T, 
                                size = "sm",
                                status = "primary",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                              )
                              ),
                              
                              column(12, plotlyOutput("reach_barchart4"))
                     )
              )
          ),
          
          box(width = 12, title =  "City-wise Average Subscription duration (Month-on-Month Trend) ", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
              
              column(12, checkboxGroupButtons(
                inputId = "reach_city_7", label = "Please select the City: ", 
                choices = unique(data_plot_F$city_new),
                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                justified = T, 
                size = "sm",
                status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              )
              ),
              
              column(12, plotlyOutput("reach_linechart2"))
              
          ), 
          
          box(width = 12, title =  "City-wise Reach Card CTR (Month-on-Month Trend) ", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
              
              column(12, checkboxGroupButtons(
                inputId = "reach_source", label = "Please select the Source: ", 
                choices = unique(data_plot_G$source),
                selected = "web",
                justified = T, 
                size = "sm",
                status = "danger",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              )
              ),
              
              column(12, checkboxGroupButtons(
                inputId = "reach_city_8", label = "Please select the City: ", 
                choices = unique(data_plot_G$city_new),
                selected = c("Hyderabad", "Mumbai_MMR", "Chennai", "Bangalore", "Delhi_NCR", "Kolkata", "Pune"),
                justified = T, 
                size = "sm",
                status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              )
              ),
              
              column(12, plotlyOutput("reach_linechart3"))
              
          ), 
          
          box(width = 12, title =  "Appointments Liquidity (Month-on-Month Trend) ", status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              
              column(12, plotlyOutput("reach_barchart5"))
              
          )
        )
      ),    
      
      
      ## 3. Content ----
      
      tabItem(
        tabName = "C",
        h1("Content Dashboard")
      ),
      
      ## 4. Feedback ----
      
      tabItem(
        tabName = "F",
        h1("Feedback Dashboard")
      )
    )
  )
)
## 4. server.R ----
server <- function(input, output, session){
  
  observeEvent(input$showSidebar, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  })
  observeEvent(input$hideSidebar, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  ## 1. Users & Transactions Charts ----
  
  output$trf_1a_combo <- renderPlotly({
    
    p <- (ggplot(data = 
                   (Country_Users %>%
                      filter(country %in% input$country) %>%
                      group_by(month_names, variable) %>%
                      summarise(`no. of users (in millions)` = round((sum(users))/1000000,3)) %>%
                      select(month_names, `no. of users (in millions)`, variable) %>%
                      ungroup()),
                 aes(x=month_names, y=`no. of users (in millions)`)) + 
            
            geom_bar(data = 
                       (Country_Users %>%
                          filter(country %in% input$country) %>%
                          group_by(month_names, variable) %>%
                          summarise(`no. of users (in millions)` = round((sum(users))/1000000,3)) %>%
                          select(month_names, `no. of users (in millions)`, variable) %>%
                          ungroup()),
                     aes(x=month_names, y=`no. of users (in millions)`, fill = variable),
                     
                     width = 10, stat = "identity", position = "dodge") +
            
            # geom_text(size=4, vjust = -1, aes(label = `no. of users (in millions)`)) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = 
                        (Channel_Country %>%
                           filter(country %in% input$country) %>%
                           group_by(month_names, channels) %>%
                           summarise(`no. of users (in millions)` = round((sum(users))/1000000,3)) %>%
                           select(month_names, channels, `no. of users (in millions)`) %>%
                           ungroup()), 
                      
                      aes(x=month_names, y=`no. of users (in millions)`, colour = channels, group = channels), size = 0.5) + 
            
            geom_point(data = 
                         (Channel_Country %>%
                            filter(country %in% input$country) %>%
                            group_by(month_names, channels) %>%
                            summarise(`no. of users (in millions)` = round((sum(users))/1000000,3)) %>%
                            select(month_names, channels, `no. of users (in millions)`) %>%
                            ungroup()), 
                       
                       aes(x=month_names, y=`no. of users (in millions)`, colour = channels, group = channels), size = 1.5))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$tra_1a_combo <- renderPlotly({
    
    # Total_Transactions_Country$variable[Total_Transactions_Country$filter == "book_practo" | Total_Transactions_Country$filter == "book_qikwell" | Total_Transactions_Country$filter == "vn_connect"] <- "Monetizable Tranxs."
    # Total_Transactions_Country$variable[is.na(Total_Transactions_Country$variable)] <- "Non-Monetizable Tranxs."
    
    p <- (ggplot(data = 
                   (Total_Transactions_Country %>%
                      filter(country %in% input$country) %>%
                      group_by(month_names) %>%
                      summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                      ungroup()),
                 aes(x=month_names, y=`no. of transactions (in thousands)`)) + 
            
            geom_bar(data = 
                       (Total_Transactions_Country %>%
                          filter(filter != "vn_forward" & filter != "vn_connect") %>%
                          filter(country %in% input$country) %>%
                          group_by(month_names) %>%
                          summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                          ungroup()),
                     aes(x=month_names, y=`no. of transactions (in thousands)`),
                     
                     width = 10, stat = "identity", fill = "deepskyblue4") +
            
            # geom_text(size=4, vjust = -1, aes(label = `no. of transactions (in thousands)`)) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = 
                        (Total_Transactions_Country %>%
                           filter(country %in% input$country) %>%
                           group_by(month_names, filter) %>%
                           summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                           ungroup()), 
                      
                      aes(x=month_names, y=`no. of transactions (in thousands)`, colour = filter, group = filter), size = 0.5) + 
            
            geom_point(data = 
                         (Total_Transactions_Country %>%
                            filter(country %in% input$country) %>%
                            group_by(month_names, filter) %>%
                            summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                            ungroup()), 
                       
                       aes(x=month_names, y=`no. of transactions (in thousands)`, colour = filter, group = filter), size = 1.5))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y")) 
    # %>%
    # layout(showlegend = T,
    # legend = list(orientation = "h")) 
    # %>%
    #   add_annotations(text = text1[[1]],
    #                   x = text2,
    #                   y = text1,
    #                   xref = "text2",
    #                   yref = "text1",
    #                   showarrow = F)
    
  })
  output$tra_1d_column <- renderPlotly({
    
    final_vpe_gmv <- final_vpe_gmv[,c(1,5:7)]
    
    final_vpe_gmv$total <- final_vpe_gmv$trans_clinic + final_vpe_gmv$trans_mm + final_vpe_gmv$trans_sa
    final_vpe_gmv$perc_clinic <- (final_vpe_gmv$trans_clinic / final_vpe_gmv$total)*100
    final_vpe_gmv$perc_mm <- (final_vpe_gmv$trans_mm / final_vpe_gmv$total)*100
    final_vpe_gmv$perc_sa <- (final_vpe_gmv$trans_sa / final_vpe_gmv$total)*100
    
    final_vpe_gmv <- final_vpe_gmv[,c(1, 6:8)]
    
    final_vpe_gmv <- melt(final_vpe_gmv, id.vars = "month")
    final_vpe_gmv$segment[final_vpe_gmv$variable == "perc_clinic"] <- "Clinic"
    final_vpe_gmv$segment[final_vpe_gmv$variable == "perc_mm"] <- "Mid Market"
    final_vpe_gmv$segment[final_vpe_gmv$variable == "perc_sa"] <- "Strategic Accounts"
    final_vpe_gmv$variable <- NULL
    colnames(final_vpe_gmv)[2] <- "Transactional Value (in percentage)"
    
    q <- (ggplot(data = final_vpe_gmv) +
            
            geom_bar(aes(x=month, y=`Transactional Value (in percentage)`, fill = segment), position="stack", stat="identity") + 
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y"))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h"))
  })
  output$tra_1b_column <- renderPlotly({
    
    Total_Transactions_Country$variable[Total_Transactions_Country$filter == "book_practo" | Total_Transactions_Country$filter == "book_qikwell" | Total_Transactions_Country$filter == "vn_forward"] <- "Monetizable Tranxs."
    # Total_Transactions_Country$variable[is.na(Total_Transactions_Country$variable)] <- "Non-Monetizable Tranxs."
    Total_Transactions_Country <- subset(Total_Transactions_Country, !is.na(Total_Transactions_Country$variable))
    Total_Transactions_Country <- Total_Transactions_Country %>%
      group_by(month_names, country, variable) %>%
      summarise(value = sum(value)) %>%
      ungroup()
    
    unique_patient_tranx$variable <- "unique_patient_transactions"
    unique_patient_tranx <- unique_patient_tranx[,c(1,2,4,3)]
    colnames(unique_patient_tranx) <- c("month_names", "country", "variable", "value")
    
    Total_Transactions_Country <- unique(rbind(Total_Transactions_Country, unique_patient_tranx))
    Total_Transactions_Country$month_names <- as.Date(Total_Transactions_Country$month_names, format = '%Y-%m-%d')
    
    p <- (ggplot(data = 
                   (Total_Transactions_Country %>%
                      filter(country %in% input$country) %>%
                      group_by(month_names, variable) %>%
                      summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                      ungroup()),
                 aes(x=month_names, y=`no. of transactions (in thousands)`)) + 
            
            geom_bar(data = 
                       (Total_Transactions_Country %>%
                          # filter(filter != "vn_attempt" & filter != "vn_connect") %>%
                          filter(country %in% input$country) %>%
                          group_by(month_names, variable) %>%
                          summarise(`no. of transactions (in thousands)` = (round((sum(value)/1000),2))) %>%
                          ungroup()),
                     aes(x=month_names, y=`no. of transactions (in thousands)`, fill = variable),
                     
                     width = 10, stat = "identity", position = "dodge") +
            
            # geom_text(size=4, vjust = -1, aes(label = `no. of transactions (in thousands)`)) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y")) 
  })
  output$tra_2a_line <- renderPlotly({
    
    p <- (ggplot() +
            
            # scale_x_date(date_minor_breaks = "5 days") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  strip.text.x = element_text(size = 12, colour = "dark blue", angle = 0),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = (melt(as.data.frame(transactions_run_rate %>%
                                                   filter(country %in% input$country) %>%
                                                   group_by(day, month_day, country) %>%
                                                   summarise(sum_book = sum(book), sum_vn = sum(vn), sum = sum(tranx)) %>%
                                                   ungroup() %>%
                                                   group_by(day, month_day) %>%
                                                   summarise(sum2_book = sum(sum_book), sum2_vn = sum(sum_vn), sum2 = sum(sum)) %>%
                                                   ungroup() %>%
                                                   group_by(month_day) %>%
                                                   mutate(cumulative_book_practo = (round((cumsum(sum2_book)/1000),2)), 
                                                          cumulative_vn_call_attempts = (round((cumsum(sum2_vn)/1000),2)), 
                                                          cumulative_transactions = (round((cumsum(sum2)/1000),2))) %>%
                                                   ungroup() %>%
                                                   select(day, month_day, cumulative_book_practo, cumulative_vn_call_attempts, cumulative_transactions) %>%
                                                   ungroup()), 
                                   id = c("day", "month_day"),
                                   measure.vars = c("cumulative_book_practo", "cumulative_vn_call_attempts", "cumulative_transactions"),
                                   variable_name = "mode"
            )
            ), 
            aes(x=day, y=value, colour = month_day, group = month_day), size = 0.5) +
            
            geom_point(data = (melt(as.data.frame(transactions_run_rate %>%
                                                    filter(country %in% input$country) %>%
                                                    group_by(day, month_day, country) %>%
                                                    summarise(sum_book = sum(book), sum_vn = sum(vn), sum = sum(tranx)) %>%
                                                    ungroup() %>%
                                                    group_by(day, month_day) %>%
                                                    summarise(sum2_book = sum(sum_book), sum2_vn = sum(sum_vn), sum2 = sum(sum)) %>%
                                                    ungroup() %>%
                                                    group_by(month_day) %>%
                                                    mutate(cumulative_book_practo = (round((cumsum(sum2_book)/1000),2)), 
                                                           cumulative_vn_call_attempts = (round((cumsum(sum2_vn)/1000),2)), 
                                                           cumulative_transactions = (round((cumsum(sum2)/1000),2))) %>%
                                                    ungroup() %>%
                                                    select(day, month_day, cumulative_book_practo, cumulative_vn_call_attempts, cumulative_transactions) %>%
                                                    ungroup()), 
                                    id = c("day", "month_day"),
                                    measure.vars = c("cumulative_book_practo", "cumulative_vn_call_attempts", "cumulative_transactions"),
                                    variable_name = "mode"
            )
            ), 
            aes(x=day, y=value, colour = month_day, group = month_day), size = 1.5))
    
    ggplotly((p + facet_grid(. ~ mode)), width = 900, height = 400, tooltip = c("x", "y"))
    
  })
  output$trf_2a_line <- renderPlotly({
    
    p <- (ggplot() +
            
            # scale_x_date(date_minor_breaks = "5 days") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = (archive_dod %>%
                                filter(channels %in% input$channel & country %in% input$country) %>%
                                group_by(day, month_day, country) %>%
                                summarise(sum = sum(users)) %>%
                                ungroup() %>%
                                group_by(day, month_day) %>%
                                summarise(sum2 = sum(sum)) %>%
                                ungroup() %>%
                                group_by(month_day) %>%
                                mutate(`no. of users (in millions)` = round((cumsum(sum2))/1000000,2)) %>%
                                ungroup()
            ), 
            aes(x=day, y=`no. of users (in millions)`, colour = month_day, group = month_day), size = 0.5) +
            
            geom_point(data = (archive_dod %>%
                                 filter(channels %in% input$channel & country %in% input$country) %>%
                                 group_by(day, month_day, country) %>%
                                 summarise(sum = sum(users)) %>%
                                 ungroup() %>%
                                 group_by(day, month_day) %>%
                                 summarise(sum2 = sum(sum)) %>%
                                 ungroup() %>%
                                 group_by(month_day) %>%
                                 mutate(`no. of users (in millions)` = round((cumsum(sum2))/1000000,2)) %>%
                                 ungroup()
            ), 
            aes(x=day, y=`no. of users (in millions)`, colour = month_day, group = month_day), size = 1.5))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  output$trf_1b_line <- renderPlotly({
    q <- (ggplot(data = 
                   
                   (Device_Country %>%
                      filter(country %in% input$country) %>%
                      group_by(month, device) %>%
                      summarise(`no. of users (in millions)` = (round((sum(users)/1000000),2))) %>%
                      select(month, device, `no. of users (in millions)`) %>%
                      ungroup()),
                 
                 aes(x=month, y=`no. of users (in millions)`)) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = 
                        
                        (Device_Country %>%
                           filter(country %in% input$country) %>%
                           group_by(month, device) %>%
                           summarise(`no. of users (in millions)` = (round((sum(users)/1000000),2))) %>%
                           select(month, device, `no. of users (in millions)`) %>%
                           ungroup()),
                      
                      aes(x=month, y=`no. of users (in millions)`, colour = device, group = device), size = 0.5) +
            
            geom_point(data = 
                         
                         (Device_Country %>%
                            filter(country %in% input$country) %>%
                            group_by(month, device) %>%
                            summarise(`no. of users (in millions)` = (round((sum(users)/1000000),2))) %>%
                            select(month, device, `no. of users (in millions)`) %>%
                            ungroup()),
                       
                       aes(x=month, y=`no. of users (in millions)`, colour = device, group = device), size = 1.5))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  output$tra_1c_line <- renderPlotly({
    r <- (ggplot() +
            
            scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = 
                        
                        (Book_Practo_Source %>%
                           filter(country %in% input$country) %>%
                           group_by(month_names, sources) %>%
                           summarise(`no. of ABS Appointments (in thousands)` = (round((sum(book_practo)/1000),2))) %>%
                           select(month_names, sources, `no. of ABS Appointments (in thousands)`) %>%
                           ungroup()),
                      
                      aes(x=month_names, y=`no. of ABS Appointments (in thousands)`, colour = sources, group = sources), size = 0.5) +
            
            geom_point(data = 
                         
                         (Book_Practo_Source %>%
                            filter(country %in% input$country) %>%
                            group_by(month_names, sources) %>%
                            summarise(`no. of ABS Appointments (in thousands)` = (round((sum(book_practo)/1000),2))) %>%
                            select(month_names, sources, `no. of ABS Appointments (in thousands)`) %>%
                            ungroup()),
                       
                       aes(x=month_names, y=`no. of ABS Appointments (in thousands)`, colour = sources, group = sources), size = 1.5))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  output$cnv_1a_column <- renderPlot({
    ggplot(data = 
             
             (Country_Transactions_Conversion_percentage %>%
                filter(country %in% input$country) %>%
                group_by(month_names) %>%
                summarise(conversion_percentage = (mean(conversion_percentage))) %>%
                select(month_names, conversion_percentage) %>%
                ungroup()),
           
           aes(x=month_names, y=conversion_percentage)) +
      
      geom_bar(width = 15, stat="identity", fill = "deepskyblue4") +
      
      geom_text(size=4, vjust = -1, aes(label = conversion_percentage)) + 
      
      scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
      
      scale_y_continuous(position = "right", limits = c(0,20)) +
      
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"))
    
    # ggplotly(p, width = 500, height = 400, tooltip = c("x", "y"))
    
  },
  height = 400,
  width = 1000
  )
  output$cnv_1b_column <- renderPlot({
    ggplot(data = 
             
             (new_conversion_percentage %>%
                filter(country %in% input$country) %>%
                group_by(month_names) %>%
                summarise(conversion_percentage = conversion_perc) %>%
                select(month_names, conversion_percentage) %>%
                ungroup()),
           
           aes(x=month_names, y=conversion_percentage)) +
      
      geom_bar(width = 15, stat="identity", fill = "darkred") +
      
      geom_text(size=4, vjust = -1, aes(label = conversion_percentage)) + 
      
      scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
      
      scale_y_continuous(position = "right", limits = c(0,20)) +
      
      theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"))
    
    # ggplotly(p, width = 500, height = 400, tooltip = c("x", "y"))
    
  },
  height = 400,
  width = 1000
  )
  output$tra_1f_table <- renderDataTable({
    subset(Transactions_Month_City_Data_Table,Transactions_Month_City_Data_Table$country %in% input$country)
  })
  output$cnv_2a_line <- renderPlotly({
    q <- (ggplot(data = 
                   
                   (City_Transactions_Conversion_percentage %>%
                      filter(city %in% input$city_conversion) %>%
                      group_by(month_names, country, city) %>%
                      summarise(conversion_percentage = conversion_percentage) %>%
                      ungroup()
                   ), 
                 
                 aes(x=month_names, y=conversion_percentage)) + 
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = (City_Transactions_Conversion_percentage %>%
                                filter(city %in% input$city_conversion) %>%
                                group_by(month_names, country, city) %>%
                                summarise(conversion_percentage = conversion_percentage) %>%
                                ungroup()
            ), 
            aes(x=month_names, y=conversion_percentage, colour = city, group = city), size = 0.5) +
            
            geom_point(data = (City_Transactions_Conversion_percentage %>%
                                 filter(city %in% input$city_conversion) %>%
                                 group_by(month_names, country, city) %>%
                                 summarise(conversion_percentage = conversion_percentage) %>%
                                 ungroup()
            ),
            aes(x=month_names, y=conversion_percentage, colour = city, group = city), size = 1.5))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$cnv_3b_line <- renderPlotly({
    p <- (ggplot(data = (Spec_Transactions_Conversion_percentage %>%
                           filter(speciality %in% input$spec_conv) %>%
                           group_by(month, speciality, transactions, users) %>%
                           summarise(conversion_percentage = conversion_percentage) %>%
                           ungroup()
    ), 
    aes(x=month, y=conversion_percentage)) +
      
      scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
      
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype")) +
      
      geom_line(data = (Spec_Transactions_Conversion_percentage %>%
                          filter(speciality %in% input$spec_conv) %>%
                          group_by(month, speciality, transactions, users) %>%
                          summarise(conversion_percentage = conversion_percentage) %>%
                          ungroup()
      ), 
      aes(x=month, y=conversion_percentage, colour = speciality, group = speciality), size = 0.5) +
      
      geom_point(data = (Spec_Transactions_Conversion_percentage %>%
                           filter(speciality %in% input$spec_conv) %>%
                           group_by(month, speciality, transactions, users) %>%
                           summarise(conversion_percentage = conversion_percentage) %>%
                           ungroup()
      ), 
      aes(x=month, y=conversion_percentage, colour = speciality, group = speciality), size = 1.5))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$cnv_3c_line <- renderPlotly({
    r <- (ggplot() +       
            
            scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            geom_line(data = (City_Spec_Transactions_Conversion_percentage %>%
                                filter(country %in% input$country) %>%
                                filter(city %in% input$city) %>%
                                group_by(month, country, city, speciality) %>%
                                summarise(mean = mean(conversion_percentage)) %>%
                                ungroup() %>%
                                group_by(month, speciality) %>%
                                summarise(`city-wise conversion_percentage` = mean(mean)) %>%
                                ungroup()
            ),
            aes(x=month, y=`city-wise conversion_percentage`, colour = speciality, group = speciality), size = 0.5) +
            
            geom_point(data = (City_Spec_Transactions_Conversion_percentage %>%
                                 filter(country %in% input$country) %>%
                                 filter(city %in% input$city) %>%
                                 group_by(month, country, city, speciality) %>%
                                 summarise(mean = mean(conversion_percentage)) %>%
                                 ungroup() %>%
                                 group_by(month, speciality) %>%
                                 summarise(`city-wise conversion_percentage` = mean(mean)) %>%
                                 ungroup()
            ),
            aes(x=month, y=`city-wise conversion_percentage`, colour = speciality, group = speciality), size = 1.5))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$cnv_3a_bar <- renderPlotly({
    
    Conversion_Spec_Current_Month$speciality <- factor(Conversion_Spec_Current_Month$speciality, 
                                                       levels = Conversion_Spec_Current_Month$speciality[order(Conversion_Spec_Current_Month$conversion_percentage, decreasing = F)])
    
    r <- (ggplot(data = Conversion_Spec_Current_Month,
                 
                 aes(x=speciality, y=conversion_percentage)) +
            
            geom_bar(stat="identity", fill = "deepskyblue4") +
            
            # geom_text(size=4, vjust = -1, aes(label = conversion_percentage)) + 
            
            scale_y_continuous(position = "right", limits = c(0,100)) +
            
            coord_flip() +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$tra_3a_line <- renderPlotly({
    r <- (ggplot() + 
            
            geom_line(data = (Cancellations_Country %>%
                                filter(country %in% input$country) %>%
                                group_by(month_names, country) %>%
                                summarise(cancellation_rate = cancellation_rate) %>%
                                ungroup()
            ),
            aes(x=month_names, y=cancellation_rate, colour = country, group = country), size = 0.5) +
            
            geom_point(data = (Cancellations_Country %>%
                                 filter(country %in% input$country) %>%
                                 group_by(month_names, country) %>%
                                 summarise(cancellation_rate = cancellation_rate) %>%
                                 ungroup()
            ),
            aes(x=month_names, y=cancellation_rate, colour = country, group = country), size = 1.5) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$tra_3b_line <- renderPlotly({
    r <-  (ggplot() + 
             
             geom_line(data = (Cancellations_City %>%
                                 filter(city %in% input$city_cancellation) %>%
                                 group_by(month_names, country, city) %>%
                                 summarise(cancellation_rate = cancellation_rate) %>%
                                 ungroup()
             ),
             aes(x=month_names, y=cancellation_rate, colour = city, group = city), size = 0.5) +
             
             geom_point(data = (Cancellations_City %>%
                                  filter(city %in% input$city_cancellation) %>%
                                  group_by(month_names, country, city) %>%
                                  summarise(cancellation_rate = cancellation_rate) %>%
                                  ungroup()
             ),
             
             aes(x=month_names, y=cancellation_rate, colour = city, group = city), size = 1.5) +
             
             scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
             
             theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
             
             theme(axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   axis.title = element_text(size = 12),
                   legend.text = element_text(size = 10),
                   legend.position = "bottom",
                   legend.justification = "center",
                   legend.direction = "horizontal",
                   legend.title = element_blank(),
                   text = element_text(family = "Palatino Linotype")))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$liq_1b_line <- renderPlotly({
    r <-  (ggplot(data = temp_2, aes(x=cumm_perc, y=transactions, colour = month, group = month)) + 
             
             geom_line(size = 0.5) +
             
             geom_point(size = 1) + 
             
             scale_x_continuous(limits = c(0,1)) + 
             
             theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
             
             theme(axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   axis.title = element_text(size = 12),
                   legend.text = element_text(size = 10),
                   legend.position = "bottom",
                   legend.justification = "center",
                   legend.direction = "horizontal",
                   legend.title = element_blank(),
                   text = element_text(family = "Palatino Linotype")))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
  })
  output$liq_1a_column <- renderPlot({
    ggplot(data = liquidity_mom,
           aes(x=month, y=liq)) +
      
      geom_bar(width = 15, stat="identity", fill = "deepskyblue4") +
      
      geom_text(size=4, vjust = -1, aes(label = liq)) + 
      
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      
      scale_y_continuous(position = "right", limits = c(0,25)) +
      
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"))
    
    # ggplotly(r, width = 500, height = 400, tooltip = c("x", "y"))
    
  },
  height = 400,
  width = 1000
  )
  output$gmv_1a_combo <- renderPlot({
    
    p1 <- ggplot(data = gmv_vpe,
                 aes(x=month, y=overall_gmv)) + 
      
      geom_bar(width = 10, stat="identity", fill = "deepskyblue4") +
      
      geom_text(size=4, vjust = -1, aes(label = (round((overall_gmv)/(10000000),2)))) + 
      
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank()) +
      
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() + 
      
      labs(y = "Overall GMV (in crores)") + 
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"))
    
    p2 <- ggplot(data = gmv_vpe,
                 aes(x=month, y=overall_gmv)) + 
      
      scale_y_continuous(limits = c(1950,2150)) +
      
      geom_line(colour = "maroon", 
                data = gmv_vpe,
                aes(x=month, y=vpe), size = 1) + theme_bw() %+replace% theme(panel.background = element_rect(fill = NA), 
                                                                             panel.grid.major = element_blank(), 
                                                                             panel.grid.minor = element_blank(), 
                                                                             panel.border = element_blank()) +
      
      scale_y_continuous(limits = c(1000,2200)) +
      
      geom_point(colour = "red", 
                 data = gmv_vpe,
                 aes(x=month, y=vpe), size = 3) + theme_bw() %+replace% theme(panel.background = element_rect(fill = NA),
                                                                              panel.grid.major = element_blank(), 
                                                                              panel.grid.minor = element_blank(), 
                                                                              panel.border = element_blank())
    
    
    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    
    # overlap the panel of 2nd plot on that of 1st plot
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, 
                         g2$grobs[[which(g2$layout$name == "panel")]],
                         pp$t,
                         pp$l, pp$b, pp$l)
    
    
    
    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    
    # draw it
    grid.draw(g)
    
  },
  height = 400,
  width = 1000
  )
  output$gmv_1b_combo <- renderPlot({
    
    p1 <- ggplot(data = 
                   (segregated_gmv %>%
                      filter(bu %in% input$bu) %>%
                      group_by(month) %>%
                      summarise(segregated_gmv = round((sum(value))/10000000,2)) %>%
                      select(month, segregated_gmv) %>%
                      ungroup()),
                 aes(x=month, y=segregated_gmv)) + 
      
      geom_bar(width = 10, stat="identity", fill = "deepskyblue4") +
      
      geom_text(size=4, vjust = -1, aes(label = segregated_gmv)) + 
      
      labs(y = "Segregated GMV (in crores)") + 
      
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank()) +
      
      theme_gdocs() + scale_fill_economist() + scale_colour_hue()
    
    p2 <- ggplot() + 
      
      
      geom_line(colour = "maroon", 
                data = 
                  (segregated_vpe %>%
                     filter(bu %in% input$bu) %>%
                     group_by(month, trans) %>%
                     summarise(multi = ((vpe*trans))) %>%
                     ungroup() %>%
                     group_by(month) %>%
                     summarise(sum_trans = sum(trans),
                               sum_multi = sum(multi)) %>%
                     ungroup() %>%
                     group_by(month) %>%
                     summarise(segregated_vpe = (sum_multi/sum_trans)) %>%
                     select(month, segregated_vpe) %>%
                     ungroup()), 
                
                aes(x=month, y=segregated_vpe), size = 1) + theme_bw() %+replace% theme(panel.background = element_rect(fill = NA),
                                                                                        panel.grid.major = element_blank(), 
                                                                                        panel.grid.minor = element_blank(), 
                                                                                        panel.border = element_blank()) +
      scale_y_continuous(limits = c(1000,10000)) +
      
      geom_point(colour = "red",
                 data = 
                   (segregated_vpe %>%
                      filter(bu %in% input$bu) %>%
                      group_by(month, trans) %>%
                      summarise(multi = ((vpe*trans))) %>%
                      ungroup() %>%
                      group_by(month) %>%
                      summarise(sum_trans = sum(trans),
                                sum_multi = sum(multi)) %>%
                      ungroup() %>%
                      group_by(month) %>%
                      summarise(segregated_vpe = (sum_multi/sum_trans)) %>%
                      select(month, segregated_vpe) %>%
                      ungroup()), 
                 
                 aes(x=month, y=segregated_vpe), size = 3) + theme_bw() %+replace% theme(panel.background = element_rect(fill = NA),
                                                                                         panel.grid.major = element_blank(), 
                                                                                         panel.grid.minor = element_blank(), 
                                                                                         panel.border = element_blank())
    
    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    
    # overlap the panel of 2nd plot on that of 1st plot
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                         pp$l, pp$b, pp$l)
    
    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    
    # draw it
    grid.draw(g)
    # ggplotly(g, width = 500, height = 400, tooltip = c("x", "y"))
    
  },
  height = 400,
  width = 1000
  )
  output$trf_1c_line <- renderPlotly({
    
    r <-  (ggplot(data = (users_profile_listing %>%
                            filter(segment %in% input$segment1) %>%
                            filter(deviceCategory %in% input$device) %>%
                            group_by(date, type) %>%
                            summarise(`No. of Users (in millions)` = round((sum(users))/1000000,3)) %>%
                            ungroup()), 
                  
                  aes(x=date, y=`No. of Users (in millions)`, colour = type, group = type)) + 
             
             geom_line(size = 1) +
             
             # geom_point(size = 2) + 
             
             scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
             
             # scale_x_continuous(limits = c(0,1)) + 
             
             theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
             
             theme(axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   axis.title = element_text(size = 12),
                   legend.text = element_text(size = 10),
                   legend.position = "bottom",
                   legend.justification = "center",
                   legend.direction = "horizontal",
                   legend.title = element_blank(),
                   text = element_text(family = "Palatino Linotype")))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
    
    
  })
  output$trf_1d_line <- renderPlotly({
    
    r <-  (ggplot(data = (users_profile_listing_tier %>%
                            filter(segment %in% input$segment2) %>%
                            filter(tier %in% input$tier) %>%
                            group_by(date, type) %>%
                            summarise(`No. of Users (in millions)` = round((sum(users))/1000000,3)) %>%
                            ungroup()), 
                  
                  aes(x=date, y=`No. of Users (in millions)`, colour = type, group = type)) + 
             
             geom_line(size = 1) +
             
             # geom_point(size = 2) + 
             
             scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
             
             # scale_x_continuous(limits = c(0,1)) + 
             
             theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
             
             theme(axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   axis.title = element_text(size = 12),
                   legend.text = element_text(size = 10),
                   legend.position = "bottom",
                   legend.justification = "center",
                   legend.direction = "horizontal",
                   legend.title = element_blank(),
                   text = element_text(family = "Palatino Linotype")))
    
    ggplotly(r, width = 1000, height = 400, tooltip = c("x", "y"))
    
    
  })
  output$tra_1g_column <- renderPlotly({
    p <- (ggplot() + 
            
            geom_bar(data = 
                       (unique_patient_tranx_city %>%
                          filter(country %in% input$country) %>%
                          group_by(month, tier) %>%
                          filter(tier %in% input$tier_unique) %>%
                          group_by(month) %>%
                          summarise(`no. of transactions (in thousands)` = (round((sum(unique_patients)/1000),2))) %>%
                          ungroup()),
                     aes(x=month, y=`no. of transactions (in thousands)`),
                     
                     width = 10, stat = "identity", fill = "dodgerblue4") +
            
            # geom_text(size=4, vjust = -1, aes(label = `no. of transactions (in thousands)`)) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y")) 
  })
  output$trf_1f_column <- renderPlotly({
    p <- (ggplot() + 
            
            geom_bar(data = 
                       (dch_archive_city %>%
                          filter(country %in% input$country) %>%
                          group_by(month, tier) %>%
                          filter(tier %in% input$tier_users) %>%
                          group_by(month) %>%
                          summarise(`no. of users (in millions)` = (round((sum(users)/1000000),2))) %>%
                          ungroup()),
                     aes(x=month, y=`no. of users (in millions)`),
                     
                     width = 10, stat = "identity", fill = "dodgerblue4") +
            
            # geom_text(size=4, vjust = -1, aes(label = `no. of transactions (in thousands)`)) +
            
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  # legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y")) 
  })
  
    
  ## 2. Sunburst Charts ----
  output$trf_1e_pie <- renderSunburst({
    
    Conversion_Spec_Current_Month$speciality <- gsub("\\-", "_", Conversion_Spec_Current_Month$speciality)
    
    if (input$tt == 'users') {
      sunburst(Conversion_Spec_Current_Month[,c(1,3)], count = T, legend = list(w = 200, h = 20, s = 20, t = 500))
    } else {
      sunburst(Conversion_Spec_Current_Month[,c(1,2)], count = T, legend = list(w = 200, h = 20, s = 20, t = 500))
    }
    
  })
  output$tra_1e_pie <- renderSunburst({
    
    Conversion_Spec_Current_Month$speciality <- gsub("\\-", "_", Conversion_Spec_Current_Month$speciality)
    
    if (input$tt == 'users') {
      sunburst(Conversion_Spec_Current_Month[,c(1,3)], count = T, legend = list(w = 200, h = 20, s = 20, t = 500))
    } else {
      sunburst(Conversion_Spec_Current_Month[,c(1,2)], count = T, legend = list(w = 200, h = 20, s = 20, t = 500))
    }
    
  })
  output$reach_sunburst1 <- renderSunburst({
    
    data_plot_B <- data_plot_B %>%
      filter(country %in% "India") %>%
      # filter(ifelse((country == "India"), city_new %in% input$sunburst_city_1, city_new == "Others")) %>%
      group_by(source, city_new) %>% 
      summarise(abs_appointments = sum(abs_appointments),
                reach_card_appointments = sum(reach_card_appointments))
    data_plot_B <- subset(data_plot_B, !is.na(source))
    data_plot_B$tree <- paste(data_plot_B$source, "-", data_plot_B$city_new)
    
    sunburst(data_plot_B[,c(5,3)], count = T, legend = list(w = 100, h = 20, s = 20, t = 20))
    
  })
  output$reach_sunburst2 <- renderSunburst({
    
    data_plot_B <- data_plot_B %>%
      filter(country %in% input$country) %>%
      filter(ifelse((country == "India"), city_new %in% input$sunburst_city_2, city_new == "Others")) %>%
      group_by(source, city_new) %>% 
      summarise(abs_appointments = sum(abs_appointments),
                reach_card_appointments = sum(reach_card_appointments))
    data_plot_B <- subset(data_plot_B, !is.na(source))
    data_plot_B$tree <- paste(data_plot_B$source, "-", data_plot_B$city_new)
    
    sunburst(data_plot_B[,c(5,4)], count = T, legend = list(w = 100, h = 20, s = 20, t = 20))
    
  })
  ## 3. Reach Dynamics Charts ----
  # output$reach_barchart1 <- renderPlotly({
  #   q <- (ggplot(data = data_plot_A %>%
  #                  filter(country %in% input$country) %>%
  #                  filter(ifelse((country == "India"), city_new %in% input$reach_city_1, city_new == "Others")) %>%
  #                  group_by(month) %>%
  #                  summarise(`transaction type (in thousands)` = round((sum(perc_abs_plus_vn_connect_patients))*100,2))) + 
  #           
  #           geom_bar(aes(x=month, y=`transaction type (in thousands)`), stat="identity", fill = "dodgerblue4") + 
  #           
  #           theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
  #           
  #           theme(axis.text.x = element_text(size = 9),
  #                 axis.text.y = element_text(size = 9),
  #                 axis.title = element_text(size = 12),
  #                 legend.text = element_text(size = 10),
  #                 legend.position = "bottom",
  #                 legend.justification = "center",
  #                 legend.direction = "horizontal",
  #                 legend.title = element_blank(),
  #                 text = element_text(family = "Palatino Linotype")) +
  #           
  #           # ggtitle("Apppointments to customers and non-customers") +
  #           
  #           # theme(legend.position = "top",
  #           #       axis.text = element_text(size=8)))
  #           
  #           scale_x_date(date_breaks = "1 month", date_labels = "%b-%y"))
  #   
  #   ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y")) %>%
  #     layout(legend = list(orientation = "h"))
  #   
  # })
  output$reach_barchart2 <- renderPlotly({
    q <- (ggplot(data = data_plot_A %>%
                   filter(country %in% input$country) %>%
                   filter(ifelse((country == "India"), city_new %in% input$reach_city_2, city_new == "Others")) %>%
                   group_by(month) %>%
                   summarise(`transaction type (%)` = mean(perc_abs_plus_vn_connect_patients)*100) %>%
                   ungroup()) + 
            
            geom_bar(aes(x=month, y=`transaction type (%)`), stat="identity", fill = "dodgerblue4") + 
            
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y"))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$reach_facet_linechart1 <- renderPlotly({
    
    data_plot_C <- data_plot_C %>%
      filter(country %in% input$country) %>%
      filter(ifelse((country == "India"), city_new %in% input$reach_city_3, city_new == "Others")) %>%
      group_by(month, source) %>% 
      summarise(abs_appointments = sum(abs_appointments),
                reach_card_appointments = sum(reach_card_appointments))
    
    data_plot_C <- subset(data_plot_C, !is.na(source))
    
    setDT(data_plot_C)
    data_plot_C <- melt(data_plot_C, id = c("month","source"))
    
    q <- (ggplot(data = data_plot_C, aes(x=month, y=value, colour = source)) + 
            
            geom_line(size = 0.5) +
            
            geom_point(size = 1.5) +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  strip.text.x = element_text(size = 12, colour = "dark blue", angle = 0),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            scale_x_date(date_breaks = "2 months", date_labels = "%b-%y"))
    
    ggplotly((q + facet_grid(.~variable)), width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  output$reach_linechart1 <- renderPlotly({
    
    data_plot_D <- data_plot_D %>%
      filter(country %in% input$country) %>%
      filter(ifelse((country == "India"), city_new %in% input$reach_city_4, city_new == "Others")) %>%
      group_by(month) %>%
      summarise(live_accounts = length(practice_id), live_subscriptions = sum(final_num_subscriptions)) %>%
      ungroup()
    
    setDT(data_plot_D)
    data_plot_D <- melt(data_plot_D, id = "month")
    
    q <- (ggplot(data = data_plot_D, aes(x=month, y=value, colour = variable)) + 
            
            geom_line(size = 0.5) +
            
            geom_point(size = 1.5) +
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  strip.text.x = element_text(size = 12, colour = "dark blue", angle = 0),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            scale_y_continuous(breaks = seq(0,10000,by=1000)) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y"))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h"))
    
  })
  output$reach_barchart3 <- renderPlotly({
    
    q <- (ggplot(data = data_plot_E %>%
                   filter(country %in% input$country) %>%
                   filter(ifelse((country == "India"), city_new %in% input$reach_city_5, city_new == "Others")) %>%
                   group_by(month, variable) %>% 
                   summarise(`no. of customers` = (sum(value)))) + 
            
            geom_bar(aes(x=month, y=`no. of customers`, fill = variable), position="stack", stat="identity") + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y"))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h"))
    
  })
  output$reach_barchart4 <- renderPlotly({
    
    q <- (ggplot(data = data_plot_E %>%
                   filter(country %in% input$country) %>%
                   filter(ifelse((country == "India"), city_new %in% input$reach_city_6, city_new == "Others")) %>%
                   group_by(month) %>% 
                   mutate(sum = (sum(value))) %>%
                   mutate(perc = ((value/sum)*100)) %>%
                   select(month, variable, perc) %>%
                   group_by(month, variable) %>%
                   summarise(`no. of customers (in %)` = sum(perc))) + 
            
            geom_bar(aes(x=month, y=`no. of customers (in %)`, fill = variable), position="stack", stat="identity") + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y"))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h"))
    
  })
  output$reach_linechart2 <- renderPlotly({
    
    data_plot_F <- data_plot_F %>%
      filter(country %in% input$country) %>%
      filter(ifelse((country == "India"), city_new %in% input$reach_city_7, city_new == "Others")) %>%
      group_by(month) %>% 
      summarise(avg_subscription_duration = sum(final_num_subscriptions*avg_subscription_duration)/sum(final_num_subscriptions)) %>%
      ungroup()
    
    q <- (ggplot(data = data_plot_F, aes(x=month, y=avg_subscription_duration, group = 1)) + 
            
            geom_line(size = 0.5) +
            
            geom_point(size = 1.5) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h"))
    
  })
  output$reach_linechart3 <- renderPlotly({
    
    data_plot_G <- data_plot_G %>%
      filter(country %in% input$country) %>%
      filter(source %in% input$reach_source) %>%
      filter(ifelse((country == "India"), city_new %in% input$reach_city_8, city_new == "Others")) %>%
      group_by(month) %>%
      summarise(ctr= ((sum(clicks)/sum(impressions))*100))
    
    q <- (ggplot(data = data_plot_G, aes(x=month, y=ctr)) + 
            
            geom_line(size = 0.5) + 
            
            geom_point(size = 1.5) +
            
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
            
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h"))
    
  })
  output$reach_barchart5 <- renderPlotly({
    
    data_plot_H <- data_plot_H %>%
      group_by(month,transactions_bucket) %>%
      summarise(count_practices = length(unique(practice_id))) %>%
      ungroup() %>%
      group_by(month) %>%
      mutate(count_practices_perc = (count_practices / sum(count_practices))*100) %>%
      ungroup()
    
    data_plot_H <- subset(data_plot_H, month >= paste0(year(Sys.Date()-60),"-", ifelse(nchar(month(Sys.Date()-60)) == 1, paste0("0",month(Sys.Date()-60)), month(Sys.Date()-365)), "-01") & 
                            month <= paste0(year(Sys.Date()),"-", ifelse(nchar(month(Sys.Date())) == 1, paste0("0",month(Sys.Date())), month(Sys.Date())), "-01"))
    
    q <- (ggplot(data = data_plot_H)  + 
            
            geom_bar(aes(x=transactions_bucket, y=count_practices_perc, fill = month), position="dodge", stat="identity") + 
            
            theme_gdocs() + scale_fill_wsj() + scale_colour_hue() +
            
            theme(axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.justification = "center",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  text = element_text(family = "Palatino Linotype")))
    
    ggplotly(q, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  ## 4. Users & Transactions Valueboxes ----
  output$users_c <- renderValueBox({
    valueBox(paste0(
      
      Users_Current_Month %>%
        filter(country %in% input$country) %>%
        group_by(month) %>%
        summarise(sum = round((sum(users))/1000000,2)) %>%
        select(sum) %>%
        ungroup(), 
      " M."),
      
      " USERS ", 
      icon = icon("users", "fa-1x"),
      subtitle = paste0("Marketplace Users  (",substr(months(update_date),1,3),"'",substr(year(update_date),3,4),")"),
      color = "blue")
  })
  output$transactions_c <- renderValueBox({
    valueBox(paste0(
      
      Transactions_Current_Month %>%
        filter(country %in% input$country) %>%
        group_by(month) %>%
        summarise(value = round((sum(value)/1000),2)) %>%
        select(value) %>%
        ungroup(), 
      " k."),
      
      " TRANSACTIONS ", 
      icon = icon("credit-card-alt"),
      subtitle = paste0("Total Transactions  (",substr(months(update_date),1,3),"'",substr(year(update_date),3,4),")"),
      color = "blue")
  })
  output$vpe_c <- renderValueBox({
    valueBox(round((vpe_current_month$vpe),2), 
             " VPE (Current Month) ",
             icon = icon("rupee"),
             subtitle = paste0("VPE  (",substr(months(update_date),1,3),"'",substr(year(update_date),3,4),")"),
             color = "blue")
  })
  output$liquidity_c <- renderValueBox({
    valueBox(paste0(round((liquidity_current_month$liq*100),2), " %"), 
             " LIQUIDITY ", 
             icon = icon("plus-square"),
             subtitle = paste0("Liquidity  (",substr(months(update_date),1,3),"'",substr(year(update_date),3,4),")"),
             color = "blue")
  })
  output$users_p <- renderValueBox({
    valueBox(paste0(
      
      Users_Previous_Month %>%
        filter(country %in% input$country) %>%
        group_by(month) %>%
        summarise(sum = round((sum(users))/1000000,2)) %>%
        select(sum) %>%
        ungroup(), 
      " M."),
      " USERS ", 
      icon = icon("users", "fa-1x"),
      subtitle = "Marketplace Users (Last Month)",
      color = "black")
  })
  output$transactions_p <- renderValueBox({
    valueBox(paste0(
      
      Transactions_Previous_Month %>%
        filter(country %in% input$country) %>%
        group_by(month) %>%
        summarise(value = round((sum(value)/1000),2)) %>%
        select(value) %>%
        ungroup(), 
      " k."),
      
      " TRANSACTIONS ", 
      icon = icon("credit-card-alt"),
      subtitle = "Total Transactions (Last Month)",
      color = "black")
  })
  output$vpe_p <- renderValueBox({
    valueBox(round((vpe_previous_month$vpe),2), 
             " VPE (Last Month)",
             icon = icon("rupee"),
             # subtitle = "Gross Marginal Value",
             color = "black")
  })
  output$liquidity_p <- renderValueBox({
    valueBox(paste0(round((liquidity_previous_month$liq*100),2), " %"), 
             " LIQUIDITY ", 
             icon = icon("plus-square"),
             subtitle = "Liquidity Score (Last Month)",
             color = "black")
  })
  ## 5. Reach Dynamics Valueboxes ----
  output$live_accounts_c <- renderValueBox({
    valueBox(paste0(
      
      data_plot_A %>%
        filter(month(data_plot_A$month) == (month(Sys.Date())), year(data_plot_A$month) == (year(Sys.Date())), data_plot_A$variable == "live_accounts") %>%
        group_by(month) %>%
        summarise(value = round((sum(value)/1000),2)) %>%
        select(value) %>%
        ungroup(), 
      " k."),
      
      " LIVE ACCOUNTS ", 
      icon = icon("credit-card-alt"),
      subtitle = "Live Accounts (Current Month)",
      color = "purple")
  })
  output$live_accounts_p <- renderValueBox({
    valueBox(paste0(
      data_plot_A %>%
        filter(month(data_plot_A$month) == (month(Sys.Date())-1), year(data_plot_A$month) == (year(Sys.Date())), data_plot_A$variable == "live_accounts") %>%
        group_by(month) %>%
        summarise(value = round((sum(value)/1000),2)) %>%
        select(value) %>%
        ungroup(), 
      " k."),
      
      " LIVE ACCOUNTS ", 
      icon = icon("credit-card-alt"),
      subtitle = "Live Accounts (Previous Month)",
      color = "maroon")
  })
  output$live_subs_c <- renderValueBox({
    valueBox(paste0(
      data_plot_A %>%
        filter(month(data_plot_A$month) == (month(Sys.Date())), year(data_plot_A$month) == (year(Sys.Date())), data_plot_A$variable == "live_subscriptions") %>%
        group_by(month) %>%
        summarise(value = round((sum(value)/1000),2)) %>%
        select(value) %>%
        ungroup(), 
      " k."),
      
      " LIVE SUBSCRIPTIONS ", 
      icon = icon("credit-card-alt"),
      subtitle = "Live Subscriptions (Current Month)",
      color = "purple")
  })
  output$live_subs_p <- renderValueBox({
    valueBox(paste0(
      data_plot_A %>%
        filter(month(data_plot_A$month) == (month(Sys.Date())-1), year(data_plot_A$month) == (year(Sys.Date())), data_plot_A$variable == "live_subscriptions") %>%
        group_by(month) %>%
        summarise(value = round((sum(value)/1000),2)) %>%
        select(value) %>%
        ungroup(), 
      " k."),
      
      " LIVE SUBSCRIPTIONS ", 
      icon = icon("credit-card-alt"),
      subtitle = "Live Subscriptions (Previous Month)",
      color = "maroon")
  })
  output$reach_card_ctr_c <- renderValueBox({
    valueBox(paste0(
      df2 %>%
        filter(month(df2$month) == (month(Sys.Date())), year(df2$month) == (year(Sys.Date()))) %>%
        group_by(month) %>%
        summarise(ctr_india = (round((ctr_india*100),2))) %>%
        select(ctr_india) %>%
        ungroup(), 
      " % "),
      
      " REACH CARD CTR ", 
      icon = icon("credit-card-alt"),
      subtitle = "Reach Card CTR (Current Month)",
      color = "purple")
  })
  output$reach_card_ctr_p <- renderValueBox({
    valueBox(paste0(
      df2 %>%
        filter(month(df2$month) == (month(Sys.Date())-1), year(df2$month) == (year(Sys.Date()))) %>%
        group_by(month) %>%
        summarise(ctr_india = (round((ctr_india*100),2))) %>%
        select(ctr_india) %>%
        ungroup(), 
      " % "),
      
      " REACH CARD CTR ", 
      icon = icon("credit-card-alt"),
      subtitle = "Reach Card CTR (Previous Month)",
      color = "maroon")
  })
  output$percent_appts_reach_card_c <- renderValueBox({
    valueBox(paste0(
      percent_appts_reach_card %>%
        filter(month(percent_appts_reach_card$month) == (month(Sys.Date())), year(percent_appts_reach_card$month) == (year(Sys.Date()))) %>%
        group_by(month) %>%
        summarise(percent = (round((percent),2))) %>%
        select(percent) %>%
        ungroup(), 
      " % "),
      
      " REACH CARD APPTS ", 
      icon = icon("credit-card-alt"),
      subtitle = "Reach Card Appts (Current Month)",
      color = "purple")
  })
  output$percent_appts_reach_card_p <- renderValueBox({
    valueBox(paste0(
      percent_appts_reach_card %>%
        filter(month(percent_appts_reach_card$month) == (month(Sys.Date())-1), year(percent_appts_reach_card$month) == (year(Sys.Date()))) %>%
        group_by(month) %>%
        summarise(percent = (round((percent),2))) %>%
        select(percent) %>%
        ungroup(), 
      " % "),
      
      " REACH CARD APPTS ", 
      icon = icon("credit-card-alt"),
      subtitle = "Reach Card Appts (Previous Month)",
      color = "maroon")
  })
}
## 5. Final EXECUTION ---- 
shinyApp(ui, server)
