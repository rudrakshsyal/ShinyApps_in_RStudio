## Module for value-boxes showing Top Metrics

top_metrics_cmUI <- function(id){
  ns <- NS(id)
  fluidRow(
    valueBoxOutput(ns("users_c"), width = 3),
    valueBoxOutput(ns("transactions_c"), width = 3),
    valueBoxOutput(ns("conversion_c"), width = 3),
    valueBoxOutput(ns("users_projected"), width = 3)
    )
  }

top_metrics_pmUI <- function(id){
  ns <- NS(id)
  fluidRow(
    valueBoxOutput(ns("users_p"), width = 3),
    valueBoxOutput(ns("transactions_p"), width = 3),
    valueBoxOutput(ns("conversion_p"), width = 3),
    valueBoxOutput(ns("transactions_projected"), width = 3)
  )
}

top_metrics_cm <- function(input, output, session, country){
  
  cm <- paste0(substr(months(Sys.Date()-1),1,3),"'",substr(year(Sys.Date()-1),3,4))
  
  users_cm <- reactive({users_current_month %>%
      filter(country %in% country()) %>%
      group_by(month) %>%
      summarise(sum = round((sum(users))/1000000,2)) %>%
      select(sum) %>%
      ungroup()})
  
  transactions_cm <- reactive({transactions_unique_monthly %>%
      filter(country %in% country()) %>%
      group_by(month) %>%
      filter(month == floor_date(Sys.Date() - 1, "month")) %>%
      summarise(value = round((sum(trans)/1000),2)) %>%
      select(value) %>%
      ungroup()})
  
  conversion_cm <- reactive({new_conversion %>%
      filter(country %in% country()) %>%
      filter(month == floor_date(Sys.Date() - 1, "month")) %>%
      group_by(month) %>%
      summarise(conversion_percentage = round((sum(unique_transactions)*100/sum(users)),2)) %>%
      select(conversion_percentage) %>%
      ungroup()})

  users_projected <- reactive({round(multiplier*users_cm(),2)})
  
  output$users_c <- renderValueBox({
    valueBox(paste0(users_cm()," M"),
      icon = icon("users", "fa-1x"),
      subtitle = paste0("Marketplace Users  (",cm,")"),
      color = "blue")
  })
  
  output$transactions_c <- renderValueBox({
    valueBox(paste0(transactions_cm()," k"),
      icon = icon("credit-card-alt"),
      subtitle = paste0("Unique Transactions  (",cm,")"),
      color = "blue")
  })
  
  output$conversion_c <- renderValueBox({
    valueBox(paste0(conversion_cm()," %"),
             icon = icon("credit-card-alt"),
             subtitle = paste0("Conversion  (",cm,")"),
             color = "blue")
  })
  
  output$users_projected <- renderValueBox({
    valueBox(paste0(users_projected()," M"),
             icon = icon("users", "fa-1x"),
             subtitle = paste0("Projected Users  (",cm,")"),
             color = "red")
  })
  
}

top_metrics_pm <- function(input, output, session, country){
  
  pm <- paste0(substr(months(floor_date(Sys.Date()-1,"month")-1),1,3),"'",substr(year(floor_date(Sys.Date()-1,"month")-1),3,4))
  cm <- paste0(substr(months(Sys.Date()-1),1,3),"'",substr(year(Sys.Date()-1),3,4))
  
  users_pm <- reactive({users_previous_month %>%
      filter(country %in% country()) %>%
      group_by(month) %>%
      summarise(sum = round((sum(users))/1000000,2)) %>%
      select(sum) %>%
      ungroup()})
  
  transactions_pm <- reactive({transactions_unique_monthly %>%
      filter(country %in% country()) %>%
      group_by(month) %>%
      filter(month == (floor_date(Sys.Date() - 1, "month") - months(1))) %>%
      summarise(value = round((sum(trans)/1000),2)) %>%
      select(value) %>%
      ungroup()})
  
  conversion_pm <- reactive({new_conversion %>%
      filter(country %in% country()) %>%
      group_by(month) %>%
      filter(month == (floor_date(Sys.Date() - 1, "month") - months(1))) %>%
      summarise(conversion_percentage = round((sum(unique_transactions)*100/sum(users)),2)) %>%
      select(conversion_percentage) %>%
      ungroup()})
  
  transactions_proj <- reactive({transactions_unique_monthly %>%
      filter(country %in% country()) %>%
      group_by(month) %>%
      filter(month == floor_date(Sys.Date() - 1, "month")) %>%
      summarise(value = round((sum(trans)/1000),2)) %>%
      select(value) %>%
      ungroup()})

  transactions_projected <- reactive({round(multiplier*transactions_proj(),2)})
  
  output$users_p <- renderValueBox({
    valueBox(paste0(users_pm()," M"),
      icon = icon("users", "fa-1x"),
      subtitle = paste0("Marketplace Users  (",pm,")"),
      color = "black")
  })
  
  output$transactions_p <- renderValueBox({
    valueBox(paste0(transactions_pm()," k"), 
      icon = icon("credit-card-alt"),
      subtitle = paste0("Unique Transactions  (",pm,")"),
      color = "black")
  })
  
  output$conversion_p <- renderValueBox({
    valueBox(paste0(conversion_pm()," %"),
             icon = icon("credit-card-alt"),
             subtitle = paste0("Conversion  (",pm,")"),
             color = "black")
  })
  
  output$transactions_projected <- renderValueBox({
    valueBox(paste0(transactions_projected()," k"), 
             icon = icon("credit-card-alt"),
             subtitle = paste0("Projected Transactions  (",cm,")"),
             color = "red")
  })
}
