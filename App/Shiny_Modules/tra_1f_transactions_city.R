## tra_1f_table -- Transaction split by City

trans_cityUI <- function(id){
  ns <- NS(id)
  tabPanel("f. by City", column(12, DT::dataTableOutput(ns("tra_1f_table"), width = 500)))
}

trans_city <- function(input,output,session,country){
  
  output$tra_1f_table <- renderDataTable({
      subset(Transactions_Month_City_Data_Table,Transactions_Month_City_Data_Table$country %in% country())
    })
}