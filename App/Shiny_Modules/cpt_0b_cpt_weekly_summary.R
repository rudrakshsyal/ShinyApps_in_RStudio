## cpt_0b_dt

cpt_weekly_summary_mrrUI <- function(id){
  ns <- NS(id)
  tabPanel("MRR", DT::dataTableOutput(ns("cpt_0b_table")))
}

cpt_weekly_summary_mrrSERVER <- function(input,output,session,country){
  
  output$cpt_0b_table <- renderDataTable({
    datatable(cpt_weekly_mrr, rownames = F, options = list(pageLength = 30, autoWidth = TRUE))
  })
}