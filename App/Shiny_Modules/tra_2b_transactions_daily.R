## Daily Run Rate - tra_2b_column
unique_trans_daily$date <- as.Date(unique_trans_daily$date)

trans_unique_dailyUI <- function(id){
  ns <- NS(id)
  tabPanel("b. Daily Run Rate",
           fluidRow(
             column(12, checkboxGroupButtons(
             inputId = ns("tier"), label = "Select Tier: ", 
             choices = unique(unique_trans_daily$tier), 
             selected = c("Bangalore", "Mumbai", "Navi Mumbai", "Thane", "Delhi", "Ghaziabad", "Faridabad", "Gurgaon", "Noida", "Pune", "Hyderabad", "Chennai", "Tier-2", "Kolkata"),
             justified = F, 
             size = "xs",
             status = "primary",
             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
           )),
           csvDownloadUI(ns("tra_2b")),
           column(12, plotlyOutput(ns("tra_2b_column")))))
}

trans_unique_7d_movingUI <- function(id){
  ns <- NS(id)
  tabPanel("c. Moving 7-day average",
           fluidRow(
             column(12, checkboxGroupButtons(
               inputId = ns("tier"), label = "Select Tier: ", 
               choices = unique(unique_trans_daily$tier), 
               selected = c("Bangalore", "Mumbai", "Navi Mumbai", "Thane", "Delhi", "Ghaziabad", "Faridabad", "Gurgaon", "Noida", "Pune", "Hyderabad", "Chennai", "Tier-2", "Kolkata"),
               justified = F, 
               size = "xs",
               status = "primary",
               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
             )), 
             csvDownloadUI(ns("tra_2bb")),
             column(12, plotlyOutput(ns("tra_2c_line"))))) 
}

trans_unique_30d_movingUI <- function(id){
  ns <- NS(id)
  tabPanel("d. Moving 30-day average",
           fluidRow(
             column(12, checkboxGroupButtons(
               inputId = ns("tier"), label = "Select Tier: ", 
               choices = unique(unique_trans_daily$tier), 
               selected = c("Bangalore", "Mumbai", "Navi Mumbai", "Thane", "Delhi", "Ghaziabad", "Faridabad", "Gurgaon", "Noida", "Pune", "Hyderabad", "Chennai", "Tier-2", "Kolkata"),
               justified = F, 
               size = "xs",
               status = "primary",
               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
             )), 
             csvDownloadUI(ns("tra_2bbb")),
             column(12, plotlyOutput(ns("tra_2d_line"))))) 
}

trans_unique_dailySERVER <- function(input, output, session, country){

data_csv <- reactive({unique_trans_daily})  

  data_2b <- reactive({
    unique_trans_daily %>%
      filter(country %in% country()) %>%
      filter(tier %in% input$tier) %>%
      filter(date >= Sys.Date()-60) %>%
      group_by(date, country) %>%
      summarise(transactions = sum(transactions))
  })
  
  output$tra_2b_column <- renderPlotly({
    
    p <- (ggplot(data_2b(),
                 aes(x=date, y=transactions)) +
            
            geom_bar(aes(fill=country),
                     stat = "identity", position = "dodge") +
            scale_x_date(date_breaks = "7 days", date_labels = "%d-%b-%y") +
            xlab("Date") + ylab("Unique Transactions") + 
            scale_fill_manual(values=c("#E69F00")) + theme(legend.position="none"))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  
  callModule(csvDownload, "tra_2b", data_csv, "daily_unique_transactions")
}

trans_unique_7d_movingSERVER <- function(input, output, session, country){
  
  start <- floor_date(Sys.Date(),"month") - months(6)
  data7d <- unique_trans_daily[unique_trans_daily$date>=start,c("date","moving_7d","country", "tier")]
  
  data_2c <- reactive({
    data7d %>%
      filter(country %in% country()) %>%
      filter(tier %in% input$tier) %>%
      group_by(date) %>%
      summarise(moving_7d = sum(moving_7d))
  })  
  
  output$tra_2c_line <- renderPlotly({
    
    p <- (ggplot(data_2c(),
                 aes(x=date, y=moving_7d)) +
            
            geom_line() +
            scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
            xlab("Date") + ylab("7 day Moving Average"))
            #scale_fill_manual(values=c("#E69F00")) + theme(legend.position="none"))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  
  callModule(csvDownload, "tra_2bb", data_2c, "rolling_7-day_average")
}

trans_unique_30d_movingSERVER <- function(input, output, session, country){
  
  data30d <- unique_trans_daily[,c("date","moving_30d","country", "tier")]
  
  data_2d <- reactive({
    data30d %>%
      filter(country %in% country()) %>%
      filter(tier %in% input$tier) %>%
      group_by(date) %>%
      summarise(moving_30d = sum(moving_30d))
  })  
  
  output$tra_2d_line <- renderPlotly({
    
    p <- (ggplot(data_2d(),
                 aes(x=date, y=moving_30d)) +
            
            geom_line() +
            scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
            xlab("Date") + ylab("30 day Moving Average"))
    #scale_fill_manual(values=c("#E69F00")) + theme(legend.position="none"))
    
    ggplotly(p, width = 1000, height = 400, tooltip = c("x", "y"))
    
  })
  
  callModule(csvDownload, "tra_2bbb", data_2d, "rolling_30-day_average")
}