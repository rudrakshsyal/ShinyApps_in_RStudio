## cpt_0a_dt

cpt_weekly_summaryUI <- function(id){
  ns <- NS(id)
  tabPanel(
    "Expansion",
    fluidRow(
    DT::dataTableOutput(ns("cpt_0a_table")),
    br(),
    tags$i("PW - Previous Week"),
    br(),
    tags$i("CW - Current Week"),
    br(),
    tags$i("Expansion - Only practices live in all days of both the weeks are considered.")
    )
  )
}

cpt_weekly_summarySERVER <- function(input,output,session,country){
  
  output$cpt_0a_table <- renderDataTable({
    datatable(cpt_weekly_expansion, rownames = F, options = list(pageLength = 30, autoWidth = TRUE))
  })
}