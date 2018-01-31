## cpt_0c_dt

cpt_monthly_expansionUI <- function(id){
  ns <- NS(id)
  tabPanel(
    "Expansion - Monthly",
    fluidRow(
    DT::dataTableOutput(ns("cpt_0c_table")),
    br(),
    tags$i("PM - Previous Month"),
    br(),
    tags$i("CM - Current Month"),
    br(),
    tags$i("Expansion - Only practices live in all days of both the months are considered.")
    )
  )
}

cpt_monthly_expansionSERVER <- function(input,output,session,country){
  
  output$cpt_0c_table <- renderDataTable({
    datatable(cpt_monthly, rownames = F, options = list(pageLength = 30, autoWidth = TRUE))
  })
}