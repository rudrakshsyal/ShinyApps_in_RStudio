## 3. ui.R ----
ui <- dashboardPage(
  title = "Marketplace Dashboard",
  skin = "black",
  
  dashboardHeader(
    
    notification_menu(), ## User defined module for Notification Menu,
    message_menu(), ## User defined module for Message Menu,
    task_menu(), ## User defined module for Task Menu,
    
    title = " Marketplace Dashboard ",
    titleWidth = 250,
    disable = F
  ),
  
  sidebarUI('sb'), ## User defined module for Sidebar,
  
  dashboardBody(
    
    includeScript("google-analytics.js"),
    
    tabItems(
      
      ## 1. Users and Transactions ----
      
      ############################ a. Traffic ----
      tabItem(
        tabName = "Traffic",
        
        textOutput("user"),
        
        top_metrics_cmUI('cm'), ## Module for Value-boxes Top metrics - Current Month
        top_metrics_pmUI('pm'), ## Module for Value-boxes Top metrics - Previous Month
        
        fluidRow(
          
          box(width = 12, title = "I. Traffic", status = "primary", solidHeader = F, collapsible = T, collapsed = F, 
              
              tags$i("TOTAL USERS - Total Practo traffic, including DCH, Consult, Diagnostics, Healthfeed, AMP and Mobile App"),
              br(),
              tags$i("MARKETPLACE USERS - Includes DCH Traffic, AMP and Mobile App"),
              hr(),
              
              tabBox(width = 28, id = "tabset_trf1",
                     
                     users_channelUI('trf_1a'), ## UI Module for Chart trf_1a
                     users_sourceUI('trf_1b'), ## UI Module for Chart trf_1b
                     users_segmentdeviceUI('trf_1c'), ## UI Module for Chart trf_1c
                     users_segmenttierUI('trf_1d'), ## UI Module for Chart trf_1d
                     #users_specUI('trf_1e'), ## UI Module for Chart trf_1e
                     users_tierUI('trf_1f') ## UI Module for Chart trf_1f
              )
          ), 
          
          box(width = 12, title = "II. Daily Run Rate", status = "primary", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_trf2",
                     
                     users_dailyUI('trf_2a'), ## UI Module for Chart trf_2a
                     users_cumulative_dailyUI('trf_2b') ## UI Module for Chart trf_2a
              )
          ),
          
          box(width = 12, title = "III. Direct Traffic", status = "primary", solidHeader = F, collapsible = T, collapsed = F,
              
              direct_city_filterUI('dt_city'),
              
              tabBox(width = 28, id = "tabset_trf3",
                     
                     users_direct_monthlyUI('trf_3a'), ## UI Module for Chart trf_2a
                     users_direct_perc_monthlyUI('trf_3b'), ## UI Module for Chart trf_2a
                     users_direct_listing_profileUI('trf_3c'),
                     users_direct_growthUI('trf_3d')
              )
          )
        )
      ),
      ############################ b. Liquidity ----
      tabItem(
        tabName = "Liquidity",
        
        fluidRow(
          
          box(width = 12, title = "I. Liquidity", status = "primary", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_liq1",
                     
                     liquidityUI('liq_1a') ## UI Module for Chart liq_1a
              )
          )
          
        )
      ),
      ############################ c. GMV / VPE ----
      tabItem(
        tabName = "GMV_VPE",
        
        fluidRow(
          
          box(width = 12, title = "I. GMV_VPE", status = "primary", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_gmv1",
                     
                     gmv_buUI('gmv_1a'), ## UI Module for Chart gmv_1a
                     vpe_buUI('gmv_1b') ## UI Module for Chart gmv_1b
              )
          )
          
        )
      ),
      ############################ d. Transactions ----
      tabItem(
        tabName = "Transactions",
        
        fluidRow(
          
          box(width = 12, title =  "I. Transactions", status = "danger", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_tra1",
                     
                     trans_modeUI('tra_1a'), ## UI Module for Chart tra_1a
                     trans_monetizeable_uniqueUI('tra_1b'), ## UI Module for Chart tra_1b
                     trans_sourceUI('tra_1c'), ## UI Module for Chart tra_1c
                     # trans_segmentUI('tra_1d'), ## UI Module for Chart tra_1d
                     # trans_specUI('tra_1e'), ## UI Module for Chart tra_1e
                     # trans_cityUI('tra_1f'), ## UI Module for Chart tra_1f
                     trans_tierUI('tra_1g') ## UI Module for Chart tra_1g
              )
          ),
          
          box(width = 12, title = "II. Transactions", status = "danger", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_tra2",
                     
                     trans_dailyUI('tra_2a'), ## UI Module for Chart tra_2a
                     trans_unique_dailyUI('tra_2b'), ## UI Module for Chart tra_2b
                     trans_unique_7d_movingUI('tra_2c'), ## UI Module for Chart tra_2c
                     trans_unique_30d_movingUI('tra_2d') ## UI Module for Chart tra_2d
              )
          ),
          
          box(width = 12, title =  "III. ABS Cancellation Rate", status = "danger", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_tra3",
                     
                     trans_abs_countryUI('tra_3a') ## UI Module for Chart tra_3a
                     #trans_abs_cityUI('tra_3b') ## UI Module for Chart tra_3b
              )
          )
        )
      ),
      ############################ e. Conversion ----
      tabItem(
        tabName = "Conversion",
        
        fluidRow(
          
          box(width = 12, title =  "I. Conversion %", status = "info", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_cnv1",
                     
                     # conversion_nonuniqueUI('cnv_1a'), ## UI Module for Chart cnv_1a
                     conversion_uniqueUI('cnv_1b'), ## UI Module for Chart cnv_1b
                     conversion_tierUI('cnv_1c'), ## UI Module for Chart cnv_1c
                     conversion_unique_sourceUI('cnv_1d') ## UI Module for Chart cnv_1d
              )
          ),

          box(width = 12, title =  "II. Daily Conversion %", status = "info", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_cnv2",
                     
                     conversion_dailyUI('cnv_2a') ## UI Module for Chart cnv_2a
              )
          )
        )
      ),
      
      ## 2. Reach Dynamics ----
      
      tabItem(
        tabName = "Reach",
        

        fluidRow(
          
        )
      ),    
      
      ## 3. CPT ----
      
      tabItem(
        tabName = "cpt_weekly",

        fluidRow(
          
          box(width = 12, title =  "MRR Weekly Tracker", status = "info", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_cpt0",
                     
                     cpt_weekly_summaryUI('cpt_0a'), ## UI Module for Chart cpt_0a
                     cpt_weekly_summary_mrrUI('cpt_0b'), ## UI Module for Chart cpt_0b
                     cpt_monthly_expansionUI('cpt_0c') ## UI Module for Chart cpt_0c
              )
          )
        )
      ),
      
      tabItem(
        tabName = "cpt_monetisation",
        
        fluidRow(
          box(width = 12, title =  "I. Transactions Monetised", status = "info", solidHeader = F, collapsible = T, collapsed = F,

              tabBox(width = 28, id = "tabset_cpt1",

                     unique_transactions_enterpriseUI('cpt_1a'), ## UI Module for Chart cpt_1a
                     transactions_monetisedUI('cpt_1b'), ## UI Module for Chart cpt_1b
                     transactions_monetised_funnelsUI('cpt_1c') ## UI Module for Chart cpt_1c
              )
          ),

          box(width = 12, title =  "II. MRR & eCPT", status = "info", solidHeader = F, collapsible = T, collapsed = F,

              tabBox(width = 28, id = "tabset_cpt2",

                     mrr_reach_cptUI('cpt_2a'), ## UI Module for Chart cpt_2a
                     ecpt_reach_cptUI('cpt_2b') ## UI Module for Chart cpt_2b
              )
          )
        )
          
      ),
      
      tabItem(
        tabName = "cpt_fqs",
        
        fluidRow(
          box(width = 12, title =  "I. FQS", status = "info", solidHeader = F, collapsible = T, collapsed = F,

              tabBox(width = 28, id = "tabset_cpt3",

                     cpt_practices_liveUI('cpt_3a'), ## UI Module for Chart cpt_3a
                     fqs_cpt_live_docsUI('cpt_3b'), ## UI Module for Chart cpt_3b
                     fqs_cpt_recos20UI('cpt_3c'), ## UI Module for Chart cpt_3c
                     fqs_cpt_reviews10UI('cpt_3d'), ## UI Module for Chart cpt_3d
                     fqs_cpt_bookUI('cpt_3e') ## UI Module for Chart cpt_3e
              )
          )
        )
      ),
      
      tabItem(
        tabName = "cpt_churn",
        
        fluidRow(
          box(width = 12, title =  "I. CPT Churn & Acquisition", status = "info", solidHeader = F, collapsible = T, collapsed = F,
              
              tabBox(width = 28, id = "tabset_cpt4",
                     
                     cpt_practice_churn_90dUI('cpt_4a'), ## UI Module for Chart cpt_4a
                     cpt_practice_churn_30dUI('cpt_4b'), ## UI Module for Chart cpt_4b
                     cpt_practice_acquisitionUI('cpt_4c') ## UI Module for Chart cpt_4c
              )
          )
        )
      ),   
      
      ## 4. Content ----
      
      tabItem(
        tabName = "Content",
        h1("Content Dashboard")
      ),
      
      ## 5. Feedback ----
      
      tabItem(
        tabName = "Feedback",
        h1("Feedback Dashboard")
      )
    )
  )
)
