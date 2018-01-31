## tra_1e_pie -- Transaction split by Speciality

Conversion_Spec_Current_Month$speciality <- gsub("\\-", "_", Conversion_Spec_Current_Month$speciality)

trans_specUI <- function(id){
  ns <- NS(id)
  tabPanel("e. by Speciality",
           column(12, 
                  radioButtons(ns("tt"), 
                               "Please select the Metric: ",
                               choiceNames = c('Traffic (Current Month)', 'Transactions (Current Month)'),
                               choiceValues = c('users', 'transactions'),
                               selected = "transactions", 
                               inline = T)),
           
           column(10, sunburstOutput(ns("tra_1e_pie"), width = "100%", height = "400px")))
}

trans_spec <- function(input,output,session,country){
  
  output$tra_1e_pie <- renderSunburst({

    if (input$tt == 'users') {
      sunburst(Conversion_Spec_Current_Month[,c(1,3)], count = T, legend = list(w = 200, h = 20, s = 20, t = 500))
    } else {
      sunburst(Conversion_Spec_Current_Month[,c(1,2)], count = T, legend = list(w = 200, h = 20, s = 20, t = 500))
    }

  })
}